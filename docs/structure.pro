%% modules
%% module(repeat).
%% module(sequel).
%% module(strings).

%% module collision
module(collision).
export(collision, data(hit)).
export(collision, type(segment)).

%% module moment
%% data/type
module(moment).
export(moment, type(moment)).

%% module calendar_system
%% data/type
module(calendar_system).
export(calendar_system, type(calendar_system)).
import(calendar_system, moment).

%% module gregorian_calendar
module(gregorian_calendar).
import(gregorian_calendar, calendar_system).

%% module calendar
module(calendar).
export(calendar, type(calendar_toolbox)).
import(calendar, calendar_system).

%% module event
module(event).
export(event, type(event)).
export(event, type(moment_segment)).
import(event, moment).


%% uses
%% module(scheduler).
%% import(module(scheduler), module(rich_event)).
%% import(module(scheduler), module(sequences)).
%% import(module(scheduler), module(strings)).
%% import(module(scheduler), module(gregorian_calendar)).
%% import(module(scheduler), module(calendar)).

%% module(sequences).
%% uses(module(sequences), module(sequence)).
%% uses(module(sequences), module(overlap).

%% module(sequence).
%% uses(module(sequence), module(rich_event)).

%% module(rich_event).
%% uses(module(rich_event), module(event)).
%% uses(module(rich_event), module(repeat)).
%% uses(module(rich_event), module(sequel)).
%% uses(module(rich_event), module(overlap)).

%% uses(module(repeat), module(event)).

%% uses(module(sequel), module(event)).

%% module(overlap).
%% uses(module(overlap, module(event)).
%% uses(module(overlap, module(collision)).
%% uses(module(overlap, data(hit)).


%% queries

%% recursive use query
depends_on(X, Y) :- import(X, Y).
depends_on(X, Y) :- import(X, Z), depends_on(Z, Y).

%% circular dependency query
circular_dependency(X, Y) :- depends_on(X, Y), depends_on(Y, X).

%% independent_module query
independent_module(X) :- module(X), not(depends_on(X, _)).

%% useless_module query
useless_module(X) :- module(X), X \== scheduler, not(depends_on(_, module(X))).
