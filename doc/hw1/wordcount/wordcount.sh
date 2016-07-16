#!/bin/bash

ocamllex wordcount.mll
ocamlc -o wordcount wordcount.ml

./wordcount < wordcount.mll
