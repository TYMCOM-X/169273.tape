






                        Marketing Opportunities Document


                                     for


                                System Profile


                                     and


                        Performance Analysis Software










    Originator:





    Approvals:













1.  Overview

    A utility is needed to gather data from each of the Osiris subsystems
    and generate both detailed and consolidated reports which can be used
    by a system administrator for resource planning and system tuning.

    This utility requires Osiris to make available the following information:

        A.  System configuration information

            Identification of all hardware and software subsystems and their
            current configuration.  Where applicable, information would include
            serial number, version and/or revision level, configuration mode,
            transfer rates, clock speed, capacity, logical connections to other
            subsystems (including network links) and current status.

        B.  Usage history information

            Usage data must be available upon request from each subsystem for
            access counts and data bytes transferred to and from each subsystem
            or object.  Information must be kept for each use (by process, task
            or job), for all usage since system reload and for all usage since
            creation of the object.

        C.  Wait queue information

            Information about all system queues must be available in order to
            measure any backlog or performance degredation due to subsystem
            bottlenecks.  This will include the CPU task queue as well as data
            or message queues for each individual process or subsystem/object.
            For example, queue length, unread message count, etc.

        D.  Activity information

            Information about all active subsystems/objects.  This would most
            likely be a current snapshot of all active and dormant (or cached)
            objects including:  Process information, open files, active object
            handles, active devices, usage data for devices, memory, stable
            store, cpu, etc.  (Something similar but much more detailed than
            the report one receives from "who" under UNIX).  Inactive resources
            should also be represented as a subset of this report.

    This information is required for performance monitoring, system tuning,
    accounting, fault identification and reliability checking as well as many
    other applications.

    


2.  Requirements

    Required:  Ability to request current configuration and status information
        from each hardware and software subsystem/object.

        The utility must be able to obtain system configuration and status
        information (via messages, SVC requests or some type of inter-process
        communication facility) from each of the Osiris subsystems and
        installed software modules or objects.  This would be correlated with
        the system configuration table to produce a configuration report.

        Error and usage statistics could be incorportated into the system
        configuration report or broken out into separate reports so that the
        system administrator could keep track of problem areas and schedule
        maintenance or replacement of hardware or software components.


    Required:  Storage space in the object directory entries and the memory-

        resident directory information of each (extended) object for read /
        write / lookup access counts and data transfer counts.

        Access count information for reads, writes, lookups, appends and data
        transfer information (presumably in bytes) to and from each object or
        Osiris subsystem are needed for any type of performance analysis.  If
        possible, space should be allocated for three sets of this information:
        current, since system startup and since object installation/creation.


    Required:  Ability to read access counts and data bytes transferred to and
        from each Osiris subsystem/object.

        This raw information is required for any realistic performance analysis
        of system activity, subsystem/object activity and throughput.  Three
        sets of activity data (current, since system startup and since object
        creation/installation) provide a historical base for comparison and
        facilitate system tuning and accounting.

        Note:  Reading the access counts must not cause an access of the object.
        However, if another object is utilized (such as a type-handler) any
        status information passing through it would be counted as data bytes
        transferred.  In the purest sense, the access information may be
        categorized as access bytes rather than as data bytes, and a separate
        set of counters may be used so as not to modify the statistical data
        while reading it.


    Required:  Ability to increment access counts and data bytes transferred to
        and from each Osiris subsystem/object.

        This process must be automatic and special privileges would be required
        to not cause the access information to be incremented.  A set of system
        parameters may also be used to turn this capability on or off for
        various reasons.  For example, if no usage accounting is desired by the
        customer, or system overhead is to be ignored in the counts, Osiris may
        want to dynamically specify which access types are to be recorded.

        A convenient central point for updating the counts would be in the
        type-handler code for each object.  The counts in the individual
        type-handlers would be maintained by the type-handler-handler, each
        handler using the same kernal code if the counts exist in the same
        object dirctory entries for each (extended) object on the system.


    Required:  Ability to reset access counts and data bytes transferred to and
        from each Osiris subsystem/object.

        Ideally the only time the counters would be reset is when an object is
        created or installed on a particular system.  The performance analysis
        or profile software would always use incremental differences in the
        counters.


    Required:  Ability to obtain information about the length and content of
        the system, subsystem or object queues.

        In order to determine how fast a subsystem/object resource is processing
        requests it is necessary to be able to read the length and content of
        individual requests in each queue.  If access counters are available,
        the content is less important except for debgging purposes.  All queues
        must be identifiable from the system configuraion tables, so that new
        subsystems will be monitored automatically such that no additional
        modifications are required each time a new subsystem is created,
        installed, modified or replaced.


    Required:  Facility for tracking individual process status for any task
        which executes on Osiris.

        Process status information is essential for monitoring the current
        state of the hardware and software subsystems/objects.  Both the
        system administrator and the software developers require accurate
        information about active processes, objects and devices.



3.  Resource Requirements Approximation

    Actual man-hours for this project would vary depending upon the detail
    desired in the reports.  However, due to the modularity of the Osiris
    system, if the modifications are made as a central point, such as the
    type-handler code, then all objects would be able to automatically take
    advantage of the specialized directory information stored on disk as part
    of the directory entry and in memory as part of the resident access data.

    Implementation of a utility which reads this information and manipulates it
    for the purpose of resource monitoring, usage accounting or system tuning
    would be very straight-forward.  The major requirement is to make sure that
    the hooks are put into Osiris as part of the object handling code.


4.  Requirement for Other Projects

    Implementation of the above requirements permits the data to be utilized in
    many many applications ranging from statistic gathering to system usage
    accounting.

  