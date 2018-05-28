# Step Execution
The execution of a step in a sequence involves two stages:
1. The configuration of all the systems involved, normally the telescope, the instrument, and sometimes GCAL.
2. The observation itself, that will produce the image.

# TCS Configuration
All of the telescope systems are configured through TCS, with the exception of some configuration items from GeMS and
Altair. The TCS configuration applied by Seqexec involves the elements described in the following paragraphs.

# Telescope configuration
* Telescope offset, for any of three defined positions (A, B and C, although only A is used)
* Target wavelength, changed to follow filter changes in the instrument.
* M2 beam: points M2 to one of the three defined positions.

## Guide Configuration
* M2 tip-tilt guide: activates the processing of tip tilt corrections in M2.
* M2 tip-tilt correction sources: defines which guiders to use. The alternatives are PWFS1, PWFS2, OIWFS and GAOS (Gems
or Altair)
* M2 coma guide: activates the processing of coma corrections in M2.
* M1 guide: activates the processing of high order corrections in M1.
* M1 source: defines which guider to use for corrections.
* Mount guide: activates the offloading of low frequency tip-tilt corrections to the mount.

## Probe Tracking
Guider probes follow a track provided by TCS to stay on target. The telescope can move (nod) between 3 positions, while
M2 can also move (chop) between those positions. TCS can be configured to update a probe target track only for certain
combinations of nod-chop positions. A probe following a target track is then controlled by two configuration elements:

* Follow flag: tells the probe to move following the target track provided by TCS.
* Nod-chop guide configuration: tells TCS for which positions of M2 and the telescope it should update the target track
of the probe.

Those configuration elements exist for PWFS1, PWFS2, OIWFS, AOWFS (Altair guide star), Canopus WFS 1 to 3 (GeMS guide
stars), and GSAOI ODGW 1 to 4 (also for GeMS).

## Probe Parking
Each probe can be parked when not in use, although sometimes that is not desirable. One such case is in a sequence that
takes several observations alternating between two positions. One of the guiders may be usable only at one of those
positions. If that is the case, the probe will not be parked in steps that uses the position that the guider cannot
reach, so it is already in position on the steps that use the other position.

## AG Configuration
* Science Fold position
* HRWFS Probe Position

## Guider Activation
Each guider can be activated or deactivated. An activated guider takes images and produces optical error measurements.

# Why Configuring TCS Is Not That Simple
The biggest complication in configuring the TCS is that certain configurations may affect the guiding. For example, if
a telescope offset is too big, it may not be possible to move the telescope while guiding. In that case instead of one
action the configuration must go through three stages:

1. Deactivate guiding. Deactivating the processing of corrections is enough, and that is how it is done in the current
Seqexec
2. Change TCS configuration
3. Reactivate guiding. The processing of corrections is always enabled at this stage, after the TCS configuration has
settled.

Other conditions that affect the TCS configurations are:

* Inability to use an OIWFS for certain steps. For example, if there is a Dark between two normal observations. In some
instruments darks are taken closing the instrument cover to block light, which will also block the light to the
instrument guider. A similar case is when taking a calibration.
* Some instrument guiders have special requirements. For example, NICI required to block the light to its internal AO
every time the telescope was moved.
* Altair and GeMS have themselves some special behaviors. It is desirable that those behaviors are encapsulated (and not
all over the code, like in the current Seqexec)

Most of those behaviors depend on how the TCS configuration will change, which in turn points to the need to compute the
set of changes before they are applied.

# Structure of the TCS Configuration Code
The code that configures TCS will be separated in at least two layers. At the top layer is the code that translates
the step parameters to a TCS configuration and deals with the details described above. The bottom layer deals with the
actual TCS, reading its state and sending commands to it. The bottom layer behavior will be defined by an interface. A
dummy implementation will be created for testing purposes.