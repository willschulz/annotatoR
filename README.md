# AnnotatoR-Flex

A flexible annotation system that supports various labeling schemes through a configurable interface. This system allows you to create custom annotation tasks with different button layouts, colors, and labeling options.

## Features

- **Flexible Button Layouts**: Support for various button arrangements (horizontal, grid, etc.)
- **Customizable Styling**: Configure button colors, sizes, shapes, and icons
- **Multiple Annotation Types**: Support for binary, multi-class, and multi-label annotations
- **JSON-based Configuration**: Easy to create and modify annotation formats
- **Batch Processing**: Upload multiple annotation tasks at once via CSV

## Directory Structure

```
annotatoR-flex/
├── app.R                 # Main Shiny application
├── process_batch.py      # Python script for batch processing
├── label_formats/        # Directory containing format configurations
│   ├── human_ai.json
│   ├── sentiment.json
│   ├── topic.json
│   └── ...
└── README.md            # This file
```

## Quick Start

1. **Start the Application**:
   ```bash
   conda activate r
   Rscript -e "shiny::runApp('app.R', port = 3839, host = '0.0.0.0')"
   ```

2. **Access the Interface**:
   - Open your browser and navigate to `http://localhost:3839`
   - Log in with your email address

## Creating Custom Annotation Formats

### Format Configuration

Create a new JSON file in the `label_formats` directory with the following structure:

```json
{
  "name": "Format Name",
  "description": "Description of the annotation task",
  "button_layout": {
    "display": "flex",
    "flexDirection": "row",
    "justifyContent": "center",
    "alignItems": "center",
    "gap": "40px",
    "buttons": [
      {
        "backgroundColor": "#4CAF50",
        "color": "white",
        "borderRadius": "50%",
        "width": "180px",
        "height": "180px",
        "padding": "0"
      }
      // ... more buttons
    ]
  },
  "annotation_labels": [
    {
      "text": "Label Text",
      "icon": "icon-name"
    }
    // ... more labels
  ]
}
```

### Button Properties

- `backgroundColor`: Button background color (hex code)
- `color`: Text color (hex code)
- `borderRadius`: Button corner radius (e.g., "50%" for circular)
- `width`: Button width
- `height`: Button height
- `padding`: Internal spacing

### Layout Properties

- `display`: Layout type ("flex" or "grid")
- `flexDirection`: Direction of button arrangement ("row" or "column")
- `justifyContent`: Horizontal alignment ("center", "start", "end", "space-between")
- `alignItems`: Vertical alignment ("center", "start", "end")
- `gap`: Space between buttons
- `flexWrap`: For multi-row layouts ("wrap" or "nowrap")

### Icons

The system uses Font Awesome icons. Available icons include:
- `thumbs-up`, `thumbs-down`
- `check`, `times`
- `star`, `star-half-alt`, `star-o`
- `smile`, `meh`, `frown`
- `user`, `robot`
- And many more (see [Font Awesome Icons](https://fontawesome.com/icons))

## Batch Processing

### CSV Format

Create a CSV file with the following columns:
- `id`: Unique identifier for the annotation task
- `annotation_instruction`: Instructions for the annotator
- `annotation_text`: The text to be annotated
- `format`: Name of the format configuration (without .json extension)

Example:
```csv
id,annotation_instruction,annotation_text,format
task1,Classify sentiment,"This product is amazing!",sentiment
task2,Is this AI-generated?,"The quick brown fox jumps.",human_ai
```

### Processing a Batch

```bash
conda activate locutus
python3 process_batch.py your_tasks.csv annotator@email.com
```

## Database Schema

The system uses a MySQL database with the following table structure:

```sql
CREATE TABLE annotatoR_table_flex (
    id VARCHAR(255) NOT NULL,
    annotation_instruction TEXT,
    annotation_html TEXT,
    annotation_labels JSON,
    button_layout JSON,
    annotator_id VARCHAR(255),
    annotation_response INT DEFAULT -1,
    annotation_flagged TINYINT DEFAULT 0,
    PRIMARY KEY (id, annotation_instruction, annotator_id)
);
```

## Example Formats

The system comes with several pre-configured formats:

1. **Human vs AI Classification**
   - Binary classification
   - Circular buttons
   - Green/Blue color scheme

2. **Sentiment Analysis**
   - Three-class classification
   - Circular buttons
   - Green/Yellow/Red color scheme

3. **Topic Classification**
   - Four-class classification
   - Grid layout
   - Multiple colors

4. **Content Moderation**
   - Binary classification
   - Circular buttons
   - Green/Red color scheme

5. **Quality Rating**
   - Three-class classification
   - Circular buttons
   - Star icons

6. **Multi-label Classification**
   - Multiple selection
   - Grid layout
   - Various colors

## Troubleshooting

1. **Buttons Not Appearing**
   - Check JSON format validity
   - Verify icon names are correct
   - Ensure button layout properties are valid

2. **Database Connection Issues**
   - Verify database credentials
   - Check network connectivity
   - Ensure table exists

3. **Batch Processing Errors**
   - Validate CSV format
   - Check format names match JSON files
   - Verify database permissions

## Contributing

To add new annotation formats:
1. Create a new JSON file in `label_formats/`
2. Test the format with a small batch
3. Document the format in this README

## License

This project is proprietary and confidential. 