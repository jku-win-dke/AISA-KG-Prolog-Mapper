:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)). 
:- set_prolog_flag(toplevel_print_anon, false).

:- dynamic current_graph/1.

current_graph('http://www.ex.org/default').

:- rdf_meta
      insert_rdf(t,t,t).

set_current_graph(Graph) :-
  retractall(current_graph(_)),
  asserta(current_graph(Graph)). % TODO asserta

insert_rdf(Subject,Predicate,Object) :- 
  current_graph(Graph),  
  rdf_assert( Subject, Predicate, Object, Graph ).
