%% modules
module(scheduler).
module(sequences).
module(sequence).
module(rich_event).
module(event).
module(repeat).
module(sequel).
module(overlap).
module(moment).
module(collision).
module(strings).
module(calendar).
module(gregorian_calendar).

%% datas
data(hit).
data(calendar_system).

%% types
type(segment).
type(collision_check).
type(moment).
type(moment_segment).
type(calendar_set).

%% uses
use(module(scheduler), module(rich_event)).
use(module(scheduler), module(sequences)).
use(module(scheduler), module(strings)).
use(module(scheduler), module(gregorian_calendar)).
use(module(scheduler), module(calendar)).

use(module(sequences), module(sequence)).
use(module(sequences), module(overlap)).

use(module(sequence), module(rich_event)).

use(module(rich_event), module(event)).
use(module(rich_event), module(repeat)).
use(module(rich_event), module(sequel)).
use(module(rich_event), module(overlap)).

use(module(event), type(moment_segment)).
use(module(event), module(moment)).

use(module(repeat), module(event)).

use(module(sequel), module(event)).

use(module(overlap), module(event)).
use(module(overlap), module(collision)).
use(module(overlap), data(hit)).

use(module(collision), data(hit)).
use(module(collision), type(segment)).
use(module(collision), type(collision_check)).
use(type(collision_check), type(segment)).

use(module(calendar), data(calendar_system)).
use(module(gregorian_calendar), data(calendar_system)).

use(module(moment), type(moment)).
use(module(moment), module(calendar)).

use(data(calendar_system), type(moment)).

use(type(moment_segment), type(moment)).

use(type(calendar_set), data(calendar_system)).



%% queries

%% recursive use query
ever_use(X, Y) :- use(X, Y).
ever_use(X, Y) :- use(X, Z), ever_use(Z, Y).

%% circular dependency query
circular_dependency(X, Y) :- ever_use(X, Y), ever_use(Y, X).

%% independent_module query
independent_module(X) :- module(X), not(ever_use(module(X), _)).

%% useless_module query
useless_module(X) :- module(X), X \== scheduler, not(ever_use(_, module(X))).
