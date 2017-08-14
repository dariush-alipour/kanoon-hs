%% modules ====================================================================

%% prolog configuration
:- discontiguous(module/1).
:- discontiguous(export/2).
:- discontiguous(import/2).
:- discontiguous(prepared/1).

%% module scheduler
module(scheduler).
import(scheduler, jar).
import(scheduler, decorator).
import(scheduler, phraser).
import(scheduler, gregorian_calendar).
import(scheduler, calendar).

%% module decorator
module(decorator).
import(decorator, jar).
import(decorator, affair).
import(decorator, collision).

%% module affair
module(affair).
%% Affair = (Race, Race, Verdict)
import(affair, race).
import(affair, verdict).
prepared(affair).

%% module jar
module(jar).
/*
Jar = {
  seed :: Seed,
  events :: [Event]
}
*/
export(jar, data(jar)).
import(jar, seed).
import(jar, event).

%% module seed
module(seed).
/*
Seed = {
  name :: String,
  race :: Race,
  verdict :: Verdict,
  period :: Period,
  repeats :: [Repeat],
  sequels :: [Sequel]
}
*/
export(seed, data(seed)).
import(seed, repeat).
import(seed, sequel).

%% module event
module(event).
/*
Event = {
  name :: String,
  period :: Period,
  isVirtual :: Bool,
  isRepeated :: Bool,
  isSequeled :: Bool
}
*/
export(event, data(event)).
export(event, type(period)).
import(event, moment).
import(event, calendar).

%% module repeat
module(repeat).
export(repeat, data(repeat)).
import(repeat, event).

%% module sequel
module(sequel).
export(sequel, data(sequel)).
import(sequel, event).

%% module calendar
module(calendar).
/*
calendar_toolbox = {
  calendars :: [CalendarSystem],
  phraser :: PhraserPowered
}
*/
export(calendar, data(calendar_toolbox)).
import(calendar, calendar_system).
import(calendar, moment).
import(calendar, phraser).

%% module gregorian_calendar
module(gregorian_calendar).
import(gregorian_calendar, calendar_system).

%% module calendar_system
%% data/type
module(calendar_system).
/*
CalendarSystem = {
  calendarId :: String,
  l10n :: Moment,
  i18n :: Moment,
  monthLength :: Moment -> Int,
  isMonthValid :: Moment -> Bool, 
  isDayValid :: Moment -> Bool,
  isLeap :: Moment -> Bool,
  offsetYear :: Moment -> Int -> Moment,
  offsetMonth :: Moment -> Int -> Moment,
  offsetDay :: Moment -> Int -> Moment
}
*/
export(calendar_system, data(calendar_system)).
import(calendar_system, moment).

%% module Race
module(race).
%% Race = Sleep | Meet | Call | Party | Know | ...
export(race, data(race)).
prepared(race).

%% module Verdict
module(verdict).
%% Verdict = Allow | Eliminate | Fit
export(verdict, data(verdict)).
prepared(verdict).

%% module moment
%% data/type
module(moment).
export(moment, data(moment)).
prepared(moment).

%% module collision
module(collision).
%% Hit = Albr | Arbl | Acbw | Awbc | Awbw
export(collision, data(hit)).
export(collision, type(segment)).
prepared(collision).

%% module phraser
module(phraser).

%% queries ====================================================================

%% recursive dependency query
depends_on(X, Y) :- import(X, Y).
depends_on(X, Y) :- import(X, Z), depends_on(Z, Y).

%% modules involved in possible circular dependency
circular_dependency(X, Y) :- depends_on(X, Y), depends_on(Y, X).

%% modules with no dependency
independent_module_(X) :- module(X), not(depends_on(X, _)).
independent_module(Xs) :- setof(X, independent_module_(X), Xs).

%% modules defined but never been used
useless_module_(X) :- module(X), X \== scheduler, not(depends_on(_, X)).
useless_module(Xs) :- setof(X, useless_module_(X), Xs).

%% modules asked for but not defined at all
missing_module_(X) :- depends_on(_, X), not(module(X)).
missing_module(Xs) :- setof(X, missing_module_(X), Xs).

%% modules with unprepared dependency
dependency_failure_module_(X) :- module(X), import(X, Y), missing_module_(Y).
dependency_failure_module_(X) :- module(X), import(X, Y), not(prepared(Y)).
dependency_failure_module(Xs) :- setof(X, dependency_failure_module_(X), Xs).

%% a suggestion on which modules to prepare first
preparable_module_(X) :- module(X), not(prepared(X)), not(dependency_failure_module_(X)).
preparable_module(Xs) :- setof(X, preparable_module_(X), Xs).

%% usage ======================================================================

%% consult("docs/outline.pro").
