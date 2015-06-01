# [Kutou]() - Kutou Language

## What is Kutou?
Kutou is a programming language similar to "Whitespace".
(http://compsoc.dur.ac.uk/whitespace/index.php)
The name "Kutou" comes from the Japanese name for the
delimiter of the sentence and phrase.


## Installation
git clone git://github.com/koji-ohki-1974/kutou
cd kutou
erlc vm.erl input.erl kutou.erl

## Usage
$ erl -noshell -s kutou main ./examples/hworld.kt -s init stop

## Syntax
Commands are composed of sequences of periods, commas and linefeeds.
It resembles "Whitespace", but uses periods in substitution for blanks,
commas in substitution for tabs.
