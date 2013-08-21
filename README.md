<img src='https://raw.github.com/hypernumbers/erlang-wtd/master/priv/images/erlang-wtd.png' />

Status
------

This software is not even Alpha. It 'runs' and it passes test, and will continue to do so, but does nothing worth doing. Ye'll huf tae wait...

Erlang WTD
----------

Erlang WTD doesn't stand for *World Total Domination* in the same way that Erlang OTP doesn't stand for *Open Telephony Platform*.

Mission
-------

To make it easy for beginners to start learning to write clustered, distributed applications on cheap commondity hardware like Raspberry Pis and low-end laptops.

Watch a presentation about it here.

<iframe src='http://prezi.com/o_-v6_pkho6k/?utm_campaign=share&utm_medium=copy' />

Definitions
-----------

An operating system (OS) is a set of libraries that run on a physical machine. When programmed against, these libraries enable your *unwritten* software to:

* write to a terminal, screen or windowing system
* persist to some class of persistant storage
* communicate via a network
* run under some class of scheduling system

Examples of operating systems are:

* Gnu/Linux
* SunOS
* Windows
* OS/X
* FreeBSD/OpenBSD/NetBSD etc

An application system (AS) is a set of libraries that run on a minimum of two physical machines. When programmed against these libraries enable your *unwritten* software to:

* fail over from one physical machine to another
* allocate and balance resources across a cluster of machines, etc, etc

Examples of application systems are:

* (Erlang) OTP
* (Erlang) WTD
* Google App Engine

Currently the resources for beginners focus on them learning OS languages (Python, Ruby, etc). People learning to write software from scratch today should be starting with AS languages.

**(Erlang) WTD is an AS designed specifically for beginners.**

Why Not Programme Against (Erlang) OTP?
---------------------------------------

Erlang OTP uses a transitive and complete trust relationship which makes it entirely unsuitable for beginners to build collaborative clusters.

In brief, if Alice lets Bob cluster his running Erlang Virtual Machine with hers, then she gives Bob the ability to run any arbritrary command on her computer - including deleting the whole hard disk. This is not good. On the internet, this is very not good indeed...

What Is (Erlang) WTD?
---------------------

(Erlang) WTD is an AS designed so that beginners can build low-risk distributed clusters with strangers they have 'met' over the internet. With (Erlang) WTD Alice can:

* let Bob execute specific software she has written on her computer
* revoke Bob's permission to do so at her convenience
* limit how frequently Bob can execute the code

With (Erlang) WTD Alice, Bob, Charlie, Dave, Erin and even Mallory and Trudy can write collaborative clustered software together over the internet.

(Erlang) WTD And (Erlang) OTP
-----------------------------

(Erlang) WTD is a close relative of (Erlang) OTP. It is designed:

* to reuse as much of (Erlang) OTP as possible
* to be as similar to (Erlang) OTP as possible
* to be as small as possible a code base

It is:

* **not** a replacement for (Erlang) OTP
* **nor** is it a fork of (Erlang) OTP
* **neither** is it better than (Erlang) OTP - just slightly different.

(Erlang) WTD Clustering
-----------------------

In (Erlang) OTP two virtual machines form a cluster if they share a common Erlang cookie. Once you have the same cookie you have a single cluster. The rights are reciprocal - if Alice has to trust Bob then Bob has to trust Alice.

In (Erlang) WTD Alice creates a public/private keypair and associates it with a list of things that can be done. She then gives this keypair to Bob (and maybe Charlie) and they then have the right to do those things on her Virtual Machine. It is not reciprocal. Alice has no rights to run software on Bob or Charlie's machines - but if they send her a PID she has the right to send messages back to that PID.

Under The Covers
----------------

The various functions calls required are bound to a URL structure that implements an erlang:apply/module/function/arguments tree.

The public/private keypairs are used to digitially sign HTTP PUT requests to that URL structure.

(Erlang) WTD EPMD
-----------------

(Erlang) WTD has a programme that acts like a standard Erlang Port Manager Daemon (except as a webservice). It needs to be running to get a cluster up.

Please see http://github.com/hypernumbers/erlang-wtd-epmd for details.

Contributing To (Erlang) WTD
-----------------------------

It is probably a bit early for anyone else to contribute as the basic core isn't working. But when it is, you will want to edit the ``BEHAVIOUR`` macro in ``wtd.erl`` and set it to ``dev`` from ``prod``.

How To Use WTD
--------------

There are two custom attributes that exposes services to wtd. Example of them can be seen in the module development.erl which is in priv/examples

The first attribute is a wtd_export attributes which says I wish other people to be able to call these fns via wtd. Its syntax is:

```erlang
    -wtd_export({mission_name, [
                                some_fn/0
                                another_fn/3
                               ]).
```

Every function tha is exported via wtd_export must also be exported. The mission name value is used to group together functions from many modules - and other erlang_wtd servers are granted rights to complete missions.

The second attributes describes how people can access OTP features across the cluster:

```erlang
    -wtd_behaviour(mission_name).
```

The module must have one of the canonical OTP behaviours to use this attribute:

* supervisor
* gen_server
* gen_event
* gen_fsm

These missions can then be bound to public/private keypairs - anyone who has those keys can invoke the fuctions.

In addition the security layer is 'leaky'. If someone calls a wtd function remotely and passes a Pid into the API they implicitly give permission for processes on the called side of the relationship to send messages to that PID.

Restrictions
------------

There are restrictions on how the API works:

* Pid's are not tranmitted over the wire - a proxy billet-doux record is sent and a proxy process is started on the remote site. Messages sent to that process are sent back.
* Fns cannot be sent over the wire - passing a fn into a function call will cause a fatal crash in the calling process.

Architecture
------------

The underlying structure of the communication architecture focusses on making it possible for learners with Raspberry PIs at home to make clusters with friends. Two machines in a cluster communicate via a proxy server - which also broadcasts details of connected machines, the services ('missions') they offer and the public keys they will accept connections from.

All communications (server to proxy and server to server) are digitically signed using public-private key pairs:

               Signed Tx          Signed Rx
    Server 1 ----------> Proxy <------------ Server 2
             ------------------------------->
                     Different Signature

EPMD And Discovery
------------------

Before using a proxy the WTD server needs to register an email with it and receive a private key in return.

After that a WTD machine ping polls the EPMD server every second and receives the current offered services back.

WTD Calls
---------

Synchronous calls are implemented asynchronously with timeouts:

    Calling      Caller       Caller   |   Proxy  |    Callee       Callee
    Process     Tx Server    Rx Server |   Cache  |   Rx Server    Tx Server
                                       |          |
    rpc ---------> Store               |          |
        <--------  PID                 |          |
    hang in                            |          |
    receive                            |          |
                  ---------------------|---->     |    Long
                  <--------------------|-----     |    Poll
                                       |    <-----|-----
                                       |    ------|---->
                                       |          | apply(M, F, A)
                               Long    |          |       ------------->
                               Poll    |    <-----|---------------------
                                -------|---->     |
                                <------|-----     |
                  <-------------       |          |
                  Ping                 |          |
     <------------                     |          |
     handle msg                        |          |
    (or timeout)                       |          |

The message signatures are:

    #signed_request{ack  = Ack,
                    body = #request{}} ----->

                                         <---- #signed_request{ack = Ack,
                                                               body = #response{}}
The Ack response has to match.

Asynchonous calls are handled as below:

    Calling      Caller       Caller   |   Proxy  |    Callee       Callee
    Process     Tx Server    Rx Server |   Cache  |   Rx Server    Tx Server
                                       |          |
    rpc --------->                     |          |
        <--------                      |          |
                  ---------------------|---->     |    Long
                  <--------------------|-----     |    Poll
                                       |    <-----|-----
                                       |    ------|---->
                                       |          | apply(M, F, A)
