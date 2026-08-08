#!/usr/bin/env python3
"""Export AnnotatoR projects from SQLite without R dependencies."""

from __future__ import annotations

import argparse
import csv
import json
import os
import re
import sqlite3
import sys
import tempfile
from contextlib import contextmanager
from datetime import datetime
from pathlib import Path
from typing import Iterator, Sequence, TextIO
from urllib.parse import quote


DEFAULT_DB = "/srv/projects/tools/annotatoR/state/annotatoR.sqlite"
FORMATS = ("csv", "tsv", "json", "jsonl")


class ExportError(Exception):
    """A user-facing export failure."""


def read_only_connection(db_path: str) -> sqlite3.Connection:
    """Open an existing SQLite database in read-only/query-only mode."""
    path = Path(db_path).expanduser().resolve()
    if not path.is_file():
        raise ExportError(f"database does not exist: {path}")

    uri = f"file:{quote(str(path), safe='/')}?mode=ro"
    connection = sqlite3.connect(uri, uri=True)
    connection.row_factory = sqlite3.Row
    connection.execute("PRAGMA busy_timeout = 5000")
    connection.execute("PRAGMA query_only = ON")
    return connection


def project_summary(connection: sqlite3.Connection) -> list[sqlite3.Row]:
    """Return projects with total and completed row counts."""
    return connection.execute(
        """
        SELECT
          project,
          COUNT(*) AS rows,
          SUM(CASE WHEN annotation_response IS NOT NULL THEN 1 ELSE 0 END)
            AS completed
        FROM items
        GROUP BY project
        ORDER BY project
        """
    ).fetchall()


def validate_projects(
    connection: sqlite3.Connection, requested: Sequence[str]
) -> None:
    """Reject unknown project names and show exact available titles."""
    available = [row["project"] for row in project_summary(connection)]
    unknown = [project for project in requested if project not in available]
    if unknown:
        choices = "\n  ".join(available)
        raise ExportError(
            "unknown project title(s): "
            + ", ".join(repr(project) for project in unknown)
            + "\nAvailable projects:\n  "
            + choices
        )


def add_in_filter(
    where: list[str],
    parameters: list[str],
    column: str,
    values: Sequence[str] | None,
) -> None:
    """Append a parameterized IN filter when values are present."""
    if not values:
        return
    placeholders = ", ".join("?" for _ in values)
    where.append(f"{column} IN ({placeholders})")
    parameters.extend(values)


def export_query(
    projects: Sequence[str],
    annotators: Sequence[str] | None,
    instruction_hashes: Sequence[str] | None,
    include_incomplete: bool,
) -> tuple[str, list[str]]:
    """Build the parameterized export query."""
    where: list[str] = []
    parameters: list[str] = []
    add_in_filter(where, parameters, "project", projects)
    add_in_filter(where, parameters, "annotator_id", annotators)
    add_in_filter(
        where,
        parameters,
        "instruction_hash",
        instruction_hashes,
    )
    if not include_incomplete:
        where.append("annotation_response IS NOT NULL")

    sql = "SELECT * FROM items"
    if where:
        sql += " WHERE " + " AND ".join(where)
    sql += " ORDER BY project, annotator_id, created_at, id"
    return sql, parameters


def infer_format(explicit_format: str | None, output: str | None) -> str:
    """Resolve an explicit format, an output suffix, or the CSV default."""
    if explicit_format:
        return explicit_format
    if output and output != "-":
        suffix = Path(output).suffix.lower().lstrip(".")
        if suffix == "ndjson":
            return "jsonl"
        if suffix in FORMATS:
            return suffix
    return "csv"


def output_filename(projects: Sequence[str], output_format: str) -> str:
    """Build a safe timestamped filename for an implicit output path."""
    slug = re.sub(r"[^a-z0-9]+", "-", projects[0].lower()).strip("-")
    if len(projects) > 1:
        slug += f"-and-{len(projects) - 1}-more"
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    return f"{slug}_{timestamp}.{output_format}"


@contextmanager
def output_stream(
    output: str,
) -> Iterator[tuple[TextIO, Path | None]]:
    """Yield stdout or an atomic temporary output stream."""
    if output == "-":
        yield sys.stdout, None
        return

    destination = Path(output).expanduser().resolve()
    destination.parent.mkdir(parents=True, exist_ok=True)
    handle = tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        newline="",
        prefix=f".{destination.name}.",
        suffix=".tmp",
        dir=destination.parent,
        delete=False,
    )
    temporary = Path(handle.name)
    try:
        yield handle, destination
        handle.flush()
        os.fsync(handle.fileno())
        handle.close()
        os.replace(temporary, destination)
    except BaseException:
        handle.close()
        temporary.unlink(missing_ok=True)
        raise


def write_delimited(
    rows: sqlite3.Cursor,
    stream: TextIO,
    delimiter: str,
) -> int:
    """Stream a cursor to CSV or TSV."""
    columns = [description[0] for description in rows.description]
    writer = csv.DictWriter(
        stream,
        fieldnames=columns,
        delimiter=delimiter,
        lineterminator="\n",
    )
    writer.writeheader()
    count = 0
    for row in rows:
        writer.writerow(dict(row))
        count += 1
    return count


def write_json(rows: sqlite3.Cursor, stream: TextIO) -> int:
    """Stream a cursor as one JSON array."""
    stream.write("[\n")
    count = 0
    for row in rows:
        if count:
            stream.write(",\n")
        json.dump(dict(row), stream, ensure_ascii=False)
        count += 1
    stream.write("\n]\n")
    return count


def write_jsonl(rows: sqlite3.Cursor, stream: TextIO) -> int:
    """Stream one JSON object per line."""
    count = 0
    for row in rows:
        json.dump(dict(row), stream, ensure_ascii=False)
        stream.write("\n")
        count += 1
    return count


def write_rows(
    rows: sqlite3.Cursor,
    stream: TextIO,
    output_format: str,
) -> int:
    """Dispatch to one streaming output writer."""
    if output_format == "csv":
        return write_delimited(rows, stream, ",")
    if output_format == "tsv":
        return write_delimited(rows, stream, "\t")
    if output_format == "json":
        return write_json(rows, stream)
    if output_format == "jsonl":
        return write_jsonl(rows, stream)
    raise ExportError(f"unsupported format: {output_format}")


def parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    argument_parser = argparse.ArgumentParser(
        prog="annotator-export",
        description=(
            "Export completed AnnotatoR rows for one or more exact project "
            "titles. SQLite is always opened read-only."
        ),
    )
    selection = argument_parser.add_mutually_exclusive_group(required=True)
    selection.add_argument(
        "--project",
        action="append",
        help="Exact project title; repeat to combine projects.",
    )
    selection.add_argument(
        "--list-projects",
        action="store_true",
        help="List available project titles and row counts, then exit.",
    )
    argument_parser.add_argument(
        "--annotator",
        action="append",
        help="Annotator ID/email to include; repeat for several.",
    )
    argument_parser.add_argument(
        "--instruction-hash",
        action="append",
        help="Instruction hash to include; repeat for several.",
    )
    argument_parser.add_argument(
        "--include-incomplete",
        action="store_true",
        help="Include rows whose annotation_response is NULL.",
    )
    argument_parser.add_argument(
        "--format",
        choices=FORMATS,
        help="Output format; otherwise infer from --output or use csv.",
    )
    argument_parser.add_argument(
        "--output",
        help=(
            "Output file. Defaults to a timestamped file in the current "
            "directory; use - for stdout."
        ),
    )
    argument_parser.add_argument(
        "--db",
        default=DEFAULT_DB,
        help=f"SQLite database path (default: {DEFAULT_DB}).",
    )
    return argument_parser


def run(arguments: argparse.Namespace) -> int:
    """Run a parsed export request."""
    with read_only_connection(arguments.db) as connection:
        if arguments.list_projects:
            print("project\trows\tcompleted")
            for row in project_summary(connection):
                print(f"{row['project']}\t{row['rows']}\t{row['completed']}")
            return 0

        projects = arguments.project
        validate_projects(connection, projects)
        output_format = infer_format(arguments.format, arguments.output)
        output = arguments.output or output_filename(projects, output_format)
        sql, parameters = export_query(
            projects=projects,
            annotators=arguments.annotator,
            instruction_hashes=arguments.instruction_hash,
            include_incomplete=arguments.include_incomplete,
        )
        rows = connection.execute(sql, parameters)
        with output_stream(output) as (stream, destination):
            count = write_rows(rows, stream, output_format)

    if output != "-":
        print(
            f"annotator-export: exported {count} rows to {destination}",
            file=sys.stderr,
        )
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    """CLI entry point."""
    try:
        return run(parser().parse_args(argv))
    except ExportError as error:
        print(f"annotator-export: error: {error}", file=sys.stderr)
        return 2
    except BrokenPipeError:
        return 0
    except sqlite3.Error as error:
        print(f"annotator-export: database error: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
