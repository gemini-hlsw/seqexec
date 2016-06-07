I wrote this program as an exercise to explore some concepts for the Sequence Executor engine. The program is an incomplete implementation of a simple game, where a ball moves inside a box at constant speed, changing direction in response to user inputs. The ball position update is driven by a simulated periodic timer. The periodic timer and the user inputs are modeled as streams of data.
The concepts I wanted to test are presented below. 
# Model the basic execution unit as a state machine that produces status updates.
The execution of a sequence can be modeled as a state machine that produces a stream of status updates (I’m using the term status update to refer to a snapshot of the sequence execution state, with the information relevant to the clients). The output stream will ultimately feed the web server to send updates to the clients. The state machine is constructed as a chain of smaller state machines. Each one takes care of an action in the sequence and produces a status update.
A second related idea is to compose the state that is passed around with the sequence execution state and the sequence commands (just the ones that affect a running sequence: `stop`, `setBreakpoint`). The action state machine will then be composed of two stages. The first one gets the input commands from the real world and updates the commands part of the state. The second one executes the action according to the previous state and the commands. For example, if the commands section has `stop==true`, then the action part should end the execution and close the output stream. That structure isolates the interaction with the real world from the rest of the processing. Part of this idea is implemented in _Game.scala_, where the state machine created by `updateSpeed` takes a player input and updates the speed direction in the state. The updated state is then passed to the state machine created by the function `tick`, which provides a new state with an updated position.
# Time control everywhere
Another concept I want to use is to treat time as an input (an idea I first read about in John Carmack’s .plan). To achieve this I need to:
Have a single source of time (and forbid having calls to system timers everywhere)
Timestamp every external event.
Use timestamps to determine time order and time concurrency.
# What do we gain ?
Introducing these concepts offers several interesting advantages:
- Modeling inputs and outputs as streams, combined with the time control, makes it easier to write unit tests. 
- It also makes possible to capture input events from the real world, and reproduce them later for debugging purposes. Because of time control, reproduction can be done at different speeds.
- It makes relatively easy to add new commands without affecting the code structure.
# What is next ?
As fun as basic streams are, they are not the solution. My plan is to use scalaz-streams. I‘ll start by introducing them to this little game, as an exercise.
There are a lot of details that I still haven’t figure out for the Sequence Executor engine. For example, this game is driven by the periodic timer: every time the timer ticks, the game machine produces an output with the new player position. At those times, the player inputs are read by polling. But, what should drive the Executor engine? The responses from the EPICS systems? What if such response takes too long? Wouldn’t that make the engine unresponsive to user commands?
