#lang ragg

drawing: rows*

rows: repeat chunk+ ";"

repeat: INTEGER

chunk: INTEGER STRING

