# task-importer

Ad-hoc tool for easier configuration of multiple Autotool tasks.
Creates a zip archive for Autotool's import interface.
(After importing the configuration must be saved through the regular configuration interface to generate a correct database signature)

## File-based
CLI interface: `task-importer <path/to/configs> <dd.mm.yyyy hh:mm> <dd.mm.yyyy hh:mm>` (or `stack run -- <path/to/configs> <dd.mm.yyyy hh:mm> <dd.mm.yyyy hh:mm>`)

The first date is the start date, and the second is the end date.

Task type is based on file extension (`.hs` or `.prolog`) or by inspecting the config (LogicTasks).

## Expression-based
Use `createTaskFiles` in [`TaskSet.hs`](src/TaskSet.hs) from `ghci`.
See [`LogicOffSemester.hs`](src/LogicOffSemester.hs) for an example task set expression.

(Currently only supports LogicTasks)
