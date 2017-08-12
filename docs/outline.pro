%% modules ====================================================================

%% prolog configuration
:- discontiguous(module/1).
:- discontiguous(export/2).
:- discontiguous(import/2).

%% module scheduler
module(scheduler).
import(scheduler, rich_event).
import(scheduler, phrase).
import(scheduler, gregorian_calendar).
import(scheduler, calendar_system).
import(scheduler, calendar).

%% module overlap
module(overlap).
import(overlap, rich_event).
import(overlap, event).
import(overlap, collision).

%% module rich_event
module(rich_event).
export(rich_event, data(rich_event)).
import(rich_event, event).
import(rich_event, repeat).
import(rich_event, sequel).
import(rich_event, overlap).

%% module event
module(event).
export(event, data(event)).
export(event, type(moment_segment)).
import(event, moment).
import(event, calendar).

%% module repeat
module(repeat).
export(repeat, data(repeat)).
import(repeat, event).

%% module sequel
module(sequel).
export(sequel, data(sequel)).
uses(sequel, event).

%% module calendar
module(calendar).
export(calendar, type(calendar_toolbox)).
import(calendar, calendar_system).
import(calendar, moment).
import(calendar, phrase).

%% module gregorian_calendar
module(gregorian_calendar).
import(gregorian_calendar, calendar_system).

%% module calendar_system
%% data/type
module(calendar_system).
export(calendar_system, type(calendar_system)).
import(calendar_system, moment).

%% module moment
%% data/type
module(moment).
export(moment, type(moment)).

%% module collision
module(collision).
export(collision, data(hit)).
export(collision, type(segment)).

%% module phrase
module(phrase).

%% queries ====================================================================

%% recursive use query
depends_on(X, Y) :- import(X, Y).
depends_on(X, Y) :- import(X, Z), depends_on(Z, Y).

%% circular dependency query
circular_dependency(X, Y) :- depends_on(X, Y), depends_on(Y, X).

%% independent_module query
independent_module(X) :- module(X), not(depends_on(X, _)).

%% useless_module query
useless_module(X) :- module(X), X \== scheduler, not(depends_on(_, X)).

%% missing_module query
missing_module(X) :- depends_on(_, X), not(module(X)).

%% usage ======================================================================

%% consult("docs/outline.pro").
