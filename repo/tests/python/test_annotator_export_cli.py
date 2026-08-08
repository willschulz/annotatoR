import contextlib
import csv
import importlib.util
import io
import json
import os
import sqlite3
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
CLI_PATH = REPO_ROOT / "inst" / "cli" / "annotator_export.py"
SPEC = importlib.util.spec_from_file_location("annotator_export_cli", CLI_PATH)
CLI = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(CLI)


class AnnotatorExportCliTest(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.db = self.root / "annotations.sqlite"
        connection = sqlite3.connect(self.db)
        connection.executescript(
            """
            CREATE TABLE items (
              id TEXT NOT NULL,
              instruction_hash TEXT NOT NULL,
              annotator_id TEXT NOT NULL,
              annotation_response TEXT,
              created_at TEXT,
              project TEXT
            );
            """
        )
        connection.executemany(
            "INSERT INTO items VALUES (?, ?, ?, ?, ?, ?)",
            [
                (
                    "ers_1",
                    "pair",
                    "perry",
                    '{"choice":"left"}',
                    "2026-01-01",
                    "Elite Rhetoric Scaling",
                ),
                (
                    "ers_2",
                    "pair",
                    "other",
                    None,
                    "2026-01-02",
                    "Elite Rhetoric Scaling",
                ),
                (
                    "cap-topic-1",
                    "topic",
                    "other",
                    '["3","7"]',
                    "2026-01-03",
                    "CAP Tweet Topic Validation",
                ),
                (
                    "fp_1",
                    "fp",
                    "will",
                    "yes",
                    "2026-01-04",
                    "False Polarization",
                ),
            ],
        )
        connection.commit()
        connection.close()

    def tearDown(self):
        self.tempdir.cleanup()

    def call(self, *arguments):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(
            stderr
        ):
            status = CLI.main([*arguments, "--db", str(self.db)])
        return status, stdout.getvalue(), stderr.getvalue()

    def test_lists_exact_project_titles_and_counts(self):
        status, stdout, stderr = self.call("--list-projects")
        self.assertEqual(status, 0)
        self.assertEqual(stderr, "")
        self.assertIn("project\trows\tcompleted", stdout)
        self.assertIn("Elite Rhetoric Scaling\t2\t1", stdout)
        self.assertIn("CAP Tweet Topic Validation\t1\t1", stdout)

    def test_defaults_to_completed_rows(self):
        output = self.root / "ers.csv"
        status, _, _ = self.call(
            "--project",
            "Elite Rhetoric Scaling",
            "--output",
            str(output),
        )
        self.assertEqual(status, 0)
        with output.open(newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))
        self.assertEqual([row["id"] for row in rows], ["ers_1"])

    def test_include_incomplete_and_repeatable_projects(self):
        output = self.root / "combined.json"
        status, _, _ = self.call(
            "--project",
            "Elite Rhetoric Scaling",
            "--project",
            "CAP Tweet Topic Validation",
            "--include-incomplete",
            "--output",
            str(output),
        )
        self.assertEqual(status, 0)
        rows = json.loads(output.read_text(encoding="utf-8"))
        self.assertEqual(len(rows), 3)
        self.assertEqual(
            {row["project"] for row in rows},
            {"Elite Rhetoric Scaling", "CAP Tweet Topic Validation"},
        )

    def test_annotator_and_instruction_filters_compose(self):
        status, stdout, _ = self.call(
            "--project",
            "Elite Rhetoric Scaling",
            "--annotator",
            "perry",
            "--instruction-hash",
            "pair",
            "--format",
            "jsonl",
            "--output",
            "-",
        )
        self.assertEqual(status, 0)
        rows = [json.loads(line) for line in stdout.splitlines()]
        self.assertEqual([row["id"] for row in rows], ["ers_1"])

    def test_all_formats_are_parseable_and_inferred_from_suffix(self):
        for output_format in CLI.FORMATS:
            with self.subTest(output_format=output_format):
                output = self.root / f"cap.{output_format}"
                status, _, _ = self.call(
                    "--project",
                    "CAP Tweet Topic Validation",
                    "--output",
                    str(output),
                )
                self.assertEqual(status, 0)
                if output_format == "csv":
                    with output.open(newline="", encoding="utf-8") as handle:
                        rows = list(csv.DictReader(handle))
                elif output_format == "tsv":
                    with output.open(newline="", encoding="utf-8") as handle:
                        rows = list(csv.DictReader(handle, delimiter="\t"))
                elif output_format == "json":
                    rows = json.loads(output.read_text(encoding="utf-8"))
                else:
                    rows = [
                        json.loads(line)
                        for line in output.read_text(
                            encoding="utf-8"
                        ).splitlines()
                    ]
                self.assertEqual(len(rows), 1)
                self.assertEqual(rows[0]["id"], "cap-topic-1")

    def test_ndjson_suffix_infers_jsonl(self):
        output = self.root / "cap.ndjson"
        status, _, _ = self.call(
            "--project",
            "CAP Tweet Topic Validation",
            "--output",
            str(output),
        )
        self.assertEqual(status, 0)
        self.assertEqual(
            json.loads(output.read_text(encoding="utf-8"))["id"],
            "cap-topic-1",
        )

    def test_unknown_project_fails_without_creating_output(self):
        output = self.root / "missing.csv"
        status, _, stderr = self.call(
            "--project",
            "Not A Project",
            "--output",
            str(output),
        )
        self.assertEqual(status, 2)
        self.assertIn("unknown project title", stderr)
        self.assertIn("Elite Rhetoric Scaling", stderr)
        self.assertFalse(output.exists())

    def test_implicit_output_is_timestamped_csv(self):
        previous = Path.cwd()
        try:
            os.chdir(self.root)
            status, _, stderr = self.call(
                "--project",
                "Elite Rhetoric Scaling",
            )
        finally:
            os.chdir(previous)
        self.assertEqual(status, 0)
        outputs = list(self.root.glob("elite-rhetoric-scaling_*.csv"))
        self.assertEqual(len(outputs), 1)
        self.assertIn(str(outputs[0]), stderr)

    def test_output_is_atomic_and_leaves_no_temporary_file(self):
        output = self.root / "nested" / "cap.csv"
        status, _, _ = self.call(
            "--project",
            "CAP Tweet Topic Validation",
            "--output",
            str(output),
        )
        self.assertEqual(status, 0)
        self.assertTrue(output.exists())
        self.assertEqual(list(output.parent.glob(".*.tmp")), [])

    def test_connection_is_query_only(self):
        connection = CLI.read_only_connection(str(self.db))
        try:
            with self.assertRaises(sqlite3.OperationalError):
                connection.execute("DELETE FROM items")
        finally:
            connection.close()


if __name__ == "__main__":
    unittest.main()
