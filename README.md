Scala Framework for Roll20 Sheets
=================================

[![CI](https://github.com/Bathtor/sheet-framework/actions/workflows/ci.yml/badge.svg)](https://github.com/Bathtor/sheet-framework/actions)

- Sheet Model: [![Maven Central](https://img.shields.io/maven-central/v/com.lkroll/roll20-sheet-model_2.13)](https://search.maven.org/artifact/com.lkroll/roll20-sheet-model_2.13)
- Sheet Framework: [![Maven Central](https://img.shields.io/maven-central/v/com.lkroll/roll20-sheet-framework_2.13)](https://search.maven.org/artifact/com.lkroll/roll20-sheet-framework_2.13)

A Framework for writing Roll20 sheets in Scala, with the HTML being generated on the JVM, and the sheet workers compiled to Javascript from Scala.js and then merged with the HTML to give a single uploadable HTML file.
Also generates CSS and `translation.json` ready for uploading.

The framework is separated into two parts: 

1. The **model** contains field declarations and can be shared between Sheet HTML Generation, Sheet Workers, and API Scripts.
2. The **framework** covers sheet HTML and CSS generation on the JVM side and Sheet Workers on the JS side.

Implemented Features
--------------------

- Field Model shared between HTML generation and sheet worker JS
- Type-safe DSL for Roll20's rollable expressions
- Sheet I18N
- Roll Templates
- Mixed source and Scalatags-generated CSS with colour palette injection
- Operation chaining for sheet worker field access/update
- Integration with Roll20 API scripts
