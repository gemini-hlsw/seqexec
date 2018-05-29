# Java EPICS Action Command Model
## Introduction
All the EPICS systems in Gemini implement the Action Command Model, defined in ICD 1A. This model describes the interface and functionality that allows a principal system to command another principal system. In particular, it is the interface used by Ocswish to provide the ability to send commands, used by applications like TCC and Seqexec. Ocswish also provides the ability to read status values from the EPICS systems, using the Baseline Attribute/Value defined in ICD 1B.

The goal of this module, the epics-acm package, is to provide the same functionalities provided by Ocswish, but for programs written in Java.

## Requirements
The functionality provided by Ocswish is implemented with several classes of objects to provide specific functionality, and a factory to produce those objects. Those classes are:
 
1. **Status Acceptor**: Keeps a collection of related attributes, each one related to a specific EPICS status channel. It offers a Listener interface to receive notifications when specific status values change.
1. **Command Sender**: Defines a specific command, related to an EPICS CAD record. It has a collection of parameters, each one associated to a specific EPICS channel (the CAD inputs). It offers to the user the functionalities to set parameters values and trigger commands. The triggering of commands can be blocking or non-blocking.
1. **Command Monitor**: An object of this class is produced when triggering a Command Sender in a non-blocking way. The Command Monitor is used to provide the user with information on the execution status of the command.
1. **Apply Sender**: This class is related with the EPICS apply and CAR records. It uses both records to trigger commands and monitor their execution. Every Command Sender is associated with an Apply Sender, mirroring the relation between EPICS apply and CAD records. A Command Sender actually triggers its EPICS CAD record through its associated Apply Sender.
1. **Service**: The Factory class used to create Status Acceptors, Apply Senders, and Command Senders.

Ocswish offers the ability to define all the Status Acceptors, Apply Senders, and Command Senders through a plain text configuration file.

The epics-acm package offers the same functionality, with equivalent Java classes. It also offers the ability to construct the different objects using the definitions from a configuration file, but the format was changed to use XML, and it was expanded to include data types for attributes and parameters.

## The Package
### EPICS Service
The package epics-acm uses packages from GMP to read and write EPICS channels. Those packages rely on the Java Channel Access libraries, JCA-CAJ. The packages used are:

* edu.gemini.epics
* edu.gemini.epics.api
* edu.gemini.epics.impl
* edu.gemini.gmp.command.records
* edu.gemini.gmp.top

The last two packages are used for the unit tests only. All the packages are available through our Artifactory repository.

### User API
The main entry point to use the epics-acm package is its Factory class, `CaService`. It offers methods to configure the underlying EPICS service, to create objects of the different classes, to explore the existing objects, to destroy them, and to shutdown the EPICS service.

Most of the classes are restricted to the package scope, The different objects are exposed to the user through public interfaces. That way, the concrete implementation can be switched, for example for testing purposes. The classes and interfaces that the user utilizes are:

1. `CaService`: the Factory class. It is a Singleton, accessed through a static method of the class.
1. `CaApplySender`: The Apply Sender interface. It allows to set the timeout for waiting for commands completion. The rest of its methods retrieve information about the Apply Sender. The objects that implement this interface are created using `CaService`.
1. `CaCommandSender`: Interface for Command Senders. It allows to add and retrieve parameters, and to trigger the command execution. The objects that implement this interface are created using `CaService`.
1. `CaParameter<T>`: Interface for parameters. It allows to retrieve information about the parameter and to set its value. Allowed types for `T` are `Int`, `Double`, `Float`, `String`, and any kind of `Enum`. The objects that implement this interface are created using `CaCommandSender`.
1. `CaCommandMonitor`: Interface to the object used to monitor the execution of a command. It is created by the different methods used to trigger a command. It offers methods to consult the execution state of a command, block until the command completes, or register a callback that will be called when the command completes.
1. `CaCommandListener`: Interface that the user must implement to receive notification when a command completes.
1. `CaStatusAcceptor`: Interface for Status Acceptors. It allows to add, retrieve and remove attributes. The objects that implement this interface are created using `CaService`.
1. `CaAttribute<T>`: Interface for attributes. It allows to retrieve information about the attribute, get its value, and register and unregister Listeners. Allowed types for `T` are `Int`, `Double`, `Float`, `String`, and any kind of `Enum`. The objects that implement this interface are created using `CaStatusAcceptor`.
1. `CaAttributeListener`: Interface that the user must implement to receive notifications when status attributes change their values.
1. `CaCommandError`, `CaCommandInProgress`, `CaCommandPostError`, `CaException`: Special exceptions that can be thrown by the epics-acm package.
1. `XMLBuilder`: Builder class, used to build Command Senders, Apply Senders and Status Acceptors from a XML configuration file.
1. `CaConfigFileConverter`: Utility to convert configuration files from the old Ocswish format to the new XML format.

The format of the XML configuration file is described in the schema file CaSchema.xsd.

Apply Senders, Command Senders, Parameters, Status Acceptors and Attributes, all are identified by a name that must be unique in their scope (i.e. all Apply Sender must have unique names; the same is valid for Command Senders and Status Acceptors. Parameters of the same Command Sender must have unique names, as well as Attributes of the same Status Acceptor). All of them have also an optional description.

All the public classes and interfaces have Javadoc documentation.

### Differences with Ocswish Implementation
The package epics-acm follows closely the functionality offered by Ocswish, with some differences:

* The attributes of Status Acceptors and the parameters of Command Senders are typed. 
* Configuration files are XML based.
* Command Acceptors have an internal link to the DIR input of their CAD. That means it is not necessary to define a dummy parameter to mark parameterless commands, as was the case with Ocswish. A consequence is that Command Acceptor now need the EPICS address of their CAD when created. 

### Implementation Details
The following table shows the concrete classes that implement each interface:

Interface | Implementation
--------- | --------------
`CaApplySender` | `CaApplySenderImpl`
`CaAttribute` | `CaAttributeImpl`
`CaCommandMonitor` | `CaCommandMonitorImpl`
`CaCommandSender` | `CaCommandSenderImpl`
`CaStatusAcceptor` | `CaStatusAcceptorImpl`
`CaParameter` | `CaParameterImpl`

The implementation of most of the classes is straightforward. The one class that merits additional explanation because of its complexity is `CaApplySenderImpl`.

`CaApplySenderImpl` must keep track of several EPICS channels to monitor the execution of a command. Given an apply record called "apply" and its associated CAR record called "car", a `CaApplySenderImpl` object must keep track of the following EPICS channels:

* apply.DIR
* apply.VAL
* apply.MESS
* car.CLID
* car.VAL
* car.OMSS

An apply record is connected to one or more CAD records. When the apply record is triggered, it triggers each CAD record in sequence. Only CAD records that have being marked for execution take action (a CAD record is marked when one of its parameters is changed, or when a MARK value is written to its DIR input). More than one CAD can be triggered, which is usually the case when applying a new configuration.

The execution of a command follows these steps:

1. The apply record is triggered by writing the value START to apply.DIR.
1. The apply record writes the value PRESET to the DIR input of each of its associated CAD records. Each CAD record in turn validate their parameters, and return a success or error value.
1. If any CAD record returns an error value, the apply record changes apply.VAL to an error value (negatives values are error codes) and puts the error message from the CAD record in apply.MESS.
1. If all the CAD records complete the PRESET stage successfully, the apply record presents a new positive value in apply.VAL. At this point the command is considered as accepted, and the new value becomes the command identification.
1. The same command id value is written to car.CLID. The channel car.VAL changes to BUSY, indicating that the command is executing.
1. If one of the CAD records reports an error, car.VAL changes to ERROR, and an error message is written to car.OMSS.
1. When all the CAD records complete their execution, car.VAL changes to IDLE.

Class `CaApplySenderImpl` takes care of the details of the execution. It triggers the apply record, monitors the apply record output to determine if the command is accepted, then monitor the CAR record to monitor when the command finishes, and retrieve the error message in case the commands ends in error. It also starts a timeout task when triggering the apply record, which will produce a timeout event if the command takes too long.

To monitor the execution of a command `CaApplySenderImpl` implements a state machine that follows this transitions:

```
        o
        |
       \|/
  .------------.
  | WaitPreset |---------------.
  `------------'               |
        |                      |
        |  apply.VAL changed   |
       \|/                     |
  .------------.               | apply.VAL changed
  | WaitStart  |               | AND apply.VAL==car.CLID
  `------------'               | AND car.VAL==BUSY
        |                      |
        |  apply.VAL==car.CLID |
        |  AND car.VAL==BUSY   |
       \|/                     |
 .--------------.              |
 |WaitCompletion|<-------------'
 `--------------'
        |
        |  car.VAL==IDLE
       \|/
       (o)
```

The processing of the events takes into account that they may arrive in a different order. The command id is captured in the transition to state WaitStart. Error transitions are not shown in the state diagram. The error conditions for each state are:

* WaitPreset: apply.VAL changes to a negative value.
* WaitStart: apply.VAL changes to a value different from the captured command id.
* WaitCompletion: apply.VAL or car.CLID change to a value different from the captured command id.
* All states: car.CLID equals the command id and car.VAL changes to ERROR. Timeout period completes.

The object `CaApplySenderImpl` creates an object of class `CaCommandMonitorImpl` when triggering a command, and returns it to the caller. The `CaCommandMonitorImpl` acts as a Future. It is passed around to the state objects, which use it to inform provide information to the user of the command execution state.

## Unit Tests
The unit tests test every public class and interface. The code includes a test simulator against which the epics-acm classes are tested. The simulator uses GMP classes to run a EPICS system.