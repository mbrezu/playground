#load "utils.cmo";;
#load "pwm.cmo";;
#load "parserTypes.cmo";;
#load "lexer.cmo";;
#load "plsqlParser.cmo";;
#load "absint.cmo";;

open PlsqlParser;;
open Lexer;;
open ParserTypes;;
open Pwm;;
open Absint;;

#use "topfind";;
#require "oUnit";;

#print_depth 10000;;
#print_length 10000;;

