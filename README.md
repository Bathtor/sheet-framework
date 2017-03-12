Scala Framework for Roll20 Sheets
=================================

A Framework for writing Roll20 sheets in Scala, with the HTML being generated on the JVM, and the sheet workers compiled to Javascript from Scala.js and then merged with the HTML to give a single uploadable HTML file.
Also generates CSS and `translation.json` ready for uploading.

Implemented Features
--------------------

- Field Model shared between HTML generation and sheet worker JS
- Type-safe DSL for Roll20's rollable expressions
- Sheet I18N
- Roll Templates
- Mixed source and Scalatags-generated CSS with colour palette injection
- Operation chaining for sheet worker field access/update

Missing Features
----------------

- Integration with Roll20 API scripts