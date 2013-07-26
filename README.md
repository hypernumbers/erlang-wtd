Erlang WTD
----------

Erlang WTD doesn't stand for *World Total Domination* in the same way that Erlang OTP doesn't stand for *Open Telephony Platform*.

Mission
-------

To make it easy for beginners to start learning to write clustered, distributed applications on cheap commondity hardware like Raspberry Pis and low-end laptops.


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

Why Not Programme against (Erlang) OTP?
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