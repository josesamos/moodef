
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moodef

<!-- badges: start -->
<!-- badges: end -->

The goal of `moodef` is to support the definition of
[*Moodle*](https://moodle.org/) elements taking advantage of the power
that R offers. In particular, in this first version, it allows the
definition of questions to be included in the question bank to define
quizzes.

To define the questions for the quizzes we can use the component for
this purpose that includes [*Moodle*](https://moodle.org/), based on
entering data through screens. It allows the import and export of
questions in various formats, including xml.

Using the `moodef` package we can define the questionnaires from R. We
have generalized 9 types of questions and simplified their definition,
so that all the types considered in this version are defined the same
and the type is deduced from the definition.

We define the questions using a function for each one or by including a
row in a data frame or csv file and interpreting them together. The
result is an xml file that we import into
[*Moodle*](https://moodle.org/).

## Installation

You can install the development version of `moodef` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/moodef")
```

## Example

This is a basic example which shows the definition of a question using
the function:

``` r
library(moodef)

qc <- question_category(category = 'Initial test',
                        copyright = 'Copyright © 2024 Universidad de Granada',
                        license = 'License Creative Commons Attribution-ShareAlike 4.0') |>
  define_question(
    question = 'What are the basic arithmetic operations?',
    answer = 'Addition, subtraction, multiplication and division.',
    a_1 = 'Addition and subtraction.',
    a_2 = 'Addition, subtraction, multiplication, division and square root.'
  )

file <- tempfile(fileext = '.xml')
qc <- qc |>
  generate_xml_file(file)
```

First, we create an object using the `question_category` function and
configure general aspects of the definition in it. Next, we define the
questions, as many as we need, using the `define_question` function. The
type of the questions is deduced from the definition.

Finally, we generate the questions in xml format, in the form of a
string or file. We show the result below.

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<quiz>
  <question type="category">
    <category> <text>$course$/top/Initial test</text> </category>
    <info format="html"> <text></text> </info>
    <idnumber></idnumber>
  </question>
  <question type="multichoice">
<name> <text>q_001_multichoice_what_are_the_basic_arithmetic_operations</text> </name>
<questiontext format="html">
  <text><![CDATA[
     <!-- Copyright © 2024 Universidad de Granada -->
     <!-- License Creative Commons Attribution-ShareAlike 4.0 -->
     <p>What are the basic arithmetic operations?</p>]]></text>
     
</questiontext>
<generalfeedback format="html"> <text></text> </generalfeedback>
<defaultgrade>1.0000000</defaultgrade>
<penalty>0.5</penalty>
<hidden>0</hidden>
<idnumber></idnumber>
<single>true</single>
<shuffleanswers>true</shuffleanswers>
<answernumbering>abc</answernumbering>
<showstandardinstruction>0</showstandardinstruction>
<correctfeedback format="moodle_auto_format"> <text>Correct.</text> </correctfeedback>
<partiallycorrectfeedback format="moodle_auto_format"> <text></text> </partiallycorrectfeedback>
<incorrectfeedback format="moodle_auto_format"> <text>Incorrect.</text> </incorrectfeedback>
<answer fraction="100" format="html">
   <text>Addition, subtraction, multiplication and division.</text>
   <feedback format="html"> <text>Correct.</text> </feedback>
</answer>
<answer fraction="-50.000000000000000" format="html">
   <text>Addition and subtraction.</text>
   <feedback format="html"> <text>Incorrect.</text> </feedback>
</answer>
<answer fraction="-50.000000000000000" format="html">
   <text>Addition, subtraction, multiplication, division and square root.</text>
   <feedback format="html"> <text>Incorrect.</text> </feedback>
</answer>
</question>
</quiz>
```

In each question we can include images that are embedded in xml. We can
define the size of the images so that they are homogeneous when
displayed in quizzes: they are automatically adapted before being
embedded in xml.
