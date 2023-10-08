---
title: "Code replacement in Erlang/OTP with Rebar3"
date: 2023-10-06T16:03:14+09:00
draft: false
tags: ["Erlang", "OTP", "Rebar3"]
---

## Introduction

### What is Erlang ?
> Erlang is a programming language used to build massively scalable soft real-time systems with requirements on high availability. Some of its uses are in telecoms, banking, e-commerce, computer telephony and instant messaging. Erlang's runtime system has built-in support for concurrency, distribution and fault tolerance. [1]

### What is OTP ?
> OTP is set of Erlang libraries and design principles providing middle-ware to develop these systems. [1]

### What is Rebar3 ?
Rebar3 is the official build tool for Erlang. [2]

### What is code replacement ? [3]
Code replacement allows us to keep our program running when we upgrade it. 

#### How does it work ?
Erlang keeps an `old` and a `current` version of the code so we can upgrade or dowgrade versions if necessary. When a third version is loaded, the newly loaded version becomes the `current` one and the previously current version becomes the `old` one.

Both the `old` and the `current` versions are valid and can run concurrently. To make our program switch to the `current` version of the code, we have to make a fully qualified call.

However, in the following recipe, we will not have to consider that because the generic abstraction that OTP gives us handles all of that for us.

#### Why is it desirable ?
Code replacement allows you to have an always running system. As cited in the "What is Erlang ?" section, this can come in handy in some areas like telecom, databases, banking systems, etc... 

It might, however, not be interesting in every situation. For example, if you run a website, it might be sufficient to simply restart it or, if your servers are behind a load balancer, to update them one by one manually.

#### How do we do it in OTP ?
At the root, we have our project. Our project can have many releases. The releases can have many applications. And these applications can have many version.

Everytime we update our applications, we will have to describe how to go from one version to the other with an appup file. Appup stands for application upgrade.

Everytime we want to create a new version of our project, we will have to create a new release and describe how to go from one version to another with a relup file. Relup stands for release upgrade.

With these files, OTP will be able to do the transition from one version to another because we tell it what to reload, what to add, what to delete etc. However, it is not easy to write correct appups, relups and updates so it is highly suggested to test the upgrades in development before applying them in production.  

## Recipe

### Ingredients
First, we will need to install the following tools:
- [Erlang](https://www.erlang.org/downloads)
- [Rebar3](https://s3.amazonaws.com/rebar3/rebar3)

The Erlang download page describes how to install it for the different operating systems so I will not enter into the details of installing it.

The rebar3 download page will directly download the rebar3 executable. Personally, I moved it under `usr/bin/rebar3` so it is accessible system-wide but you could also put it elsewhere and type the path under which it is located every time you want to use the rebar3 command. 

### Create a project
After installing the required tools, we will need a project to work with. To create a new project we can simply type:
```sh
rebar3 new release
```

This will create a release project for us and populate it under a folder named `myapp`. You could also have given it a name by writing it, separated by a space after `release`.

For the rest of this tutorial the `myapp` folder will be considered as being the root folder.

If you want to learn about the different type of projects you can generate, [click here](http://rebar3.org/docs/basic_usage/#new-app-or-release).

### Change the relx mode
By default, the `rebar.config` file sets the relx mode to dev however this will cause us some trouble when will need to generate a relup because the different versions of our app will point to the latest build which means that we will lack the files for all the old versions.

To fix this, open the `rebar.config` file. And edit the `{mode, dev}` tuple which is part of the relx configuration to `{mode, prod}`. At the moment I am writing this recipe, it is located at the eight line.

### Link the application with the supervisor and your gen_server
To test our project, we will need to modify our app a little bit. First we will add a gen_server we can communicate to and then we will tell our supervisor to supervise our gen_server.

#### Adding a gen_server
To add a gen_server, create a file under `/apps/myapp/src/myapp_serv.erl`.
I will fill mine with this but you could put anything you want in it:

```erl
-module(myapp_serv).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([hello/1]). % Added by myself
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-define(SERVER, ?MODULE).


hello(Pid) ->
    gen_server:call(Pid, hello). % Added by myself

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(hello, _From, State) -> % Added by myself
    {reply, hello, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

```

Most of it was generated by the Erlang VScode extension. I have only added a function called `hello/1` which allows us to send an `hello` call to the server that will reply us with `hello` in return.

#### Linking the supervisor and the gen_server
To link your supervisor with the gen_server, you could do this:

```erl
%%%-------------------------------------------------------------------
%% @doc myapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(myapp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  ChildSpec = #{id => myapp,
    start => {myapp_serv, start_link, []},
    restart => temporary,
    shutdown => 1000,
    type => worker,
    modules => [myapp_serv]},

  {ok, {#{strategy => simple_one_for_one,
    intensity => 3,
    period => 60},
    [ChildSpec]}
  }.


%% internal functions
```

Here, a `simple_one_for_one` strategy is used because I like being able to spawn a new child as I wish because I find it easier to work with. That said, it is not a requirement.

### Compile your code and build a release of it
To compile your code and release it, run this command:
```sh
rebar3 compile && rebar3 release
```

These commands are pretty self explainatory. The first one will generate a build of the project and the second one will produce a release of the project. Releases can be seen as a version of your entire project.

#### Test your code
To test our code, we want to run the release we have just created. To do this, we can run this command: `./_build/default/rel/myapp/bin/myapp-0.1.0 console`. This will launch an Erlang shell with our latest release loaded in it.

Once in the Erlang shell, if you have copied my code until now, you should be able to run these command to check that everything is correct.

```erl
{ok, Pid} = supervisor:start_child(myapp_sup, []).
```

```erl
myapp_serv:hello(Pid).
```

And you should get this answer `hello`.

I would suggest to keep this shell running while during the following steps as it will be easier to show that our update was correclty done.

### Adding a new functionality to our gen_server
As I want to keep it minimal, I will just add new function called `hey/1` that will behave the same as the `hello/1` function except that it will reply with `hey` instead of `hello`.

My gen_server now looks like this:
```erl
-module(myapp_serv).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([hello/1, hey/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-define(SERVER, ?MODULE).


hello(Pid) ->
    gen_server:call(Pid, hello).

hey(Pid) ->
    gen_server:call(Pid, hey).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(hello, _From, State) ->
    {reply, hello, State};

handle_call(hey, _From, State) ->
    {reply, hey, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

```

#### Update the version of the .app file
As we have updated our app by adding a new function to our gen_server and we are satisfied with it, we need to bump the version that is written in the `apps/myapp/src/myapp.app.src` file. To do that, we can replace the `"0.1.0"` version string with a higher version string. I will choose `"0.2.0"` for this example.

#### Create an appup file
An appup file allows one to describe the steps that are necessary to go from one version of an application to an other. As we bumped the version of our app, we will need to create one.

Create the appup file at `/apps/myapp/src/myapp.appup.src`.
Here, as the changes I have made are minimal, the appup file's content will be quite simple:
```
{"0.2.0", % New version
 [{"0.1.0", [{load_module, myapp_serv}]}], % steps for 0.1.0 -> 0.2.0
 [{"0.1.0", [{load_module, myapp_serv}]}]  % steps for 0.2.0 -> 0.1.0
}.
```
As I did not change the state of my gen_server I can simply load my gen_server's module.

If you do not know how to write an appup file, you can consult the [appup cookbook](https://www.erlang.org/doc/design_principles/appup_cookbook.html)

#### Copy you appup file in your _build directory
Copy your `/apps/myapp/src/myapp.appup.src` to `_build/default/lib/myapp/ebin/myapp.appup` with the following command:
```sh
cp /apps/myapp/src/myapp.appup.src _build/default/lib/myapp/ebin/myapp.appup
```

### Update the version of the rebar.config file
Now, that we have done the changes we wanted to in our app(s), we also want to create a new release of our project so that we can later upgrade it.

To do that, open the `rebar.config` file and also replace the `"0.1.0"` version string with a higher version string. I will choose `"0.2.0"` for this example.

### Compile and release
```sh
rebar3 compile && rebar3 release
```

### Create a relup file
A relup file allows one to describe the steps that are necessary to go from one release of an application to an other. As we bumped the version of our project, we will need to create one.

To create a relup file to upgrade our current release, type this command:
```sh
rebar3 relup -n myapp -v "0.2.0" -u "0.1.0"
```
This command tells rebar3 to generate the relup file necessary to go from `0.1.0` to `0.2.0`.

### Pack your release
To load your newest release, you will need to pack it in an archive.

To do this, use this command:
```sh
rebar3 tar -n myapp -v "0.2.0"
```

### Move the packed release to the release folder
To make the upgrade, we will need to move our packed release from where it was generated to the release folder. To do this, type:
```sh
mv _build/default/rel/myapp/myapp-0.2.0.tar.gz _build/default/rel/myapp/releases/0.2.0/myapp.tar.gz
```

### Upgrade your app
Now that our packed release is at the correct location, we can proceed to make the upgrade. To do this type:

```sh
_build/default/rel/myapp/bin/myapp-0.1.0 upgrade "0.2.0"
```

This command tells the 0.1.0 release to upgrade to the 0.2.0 release.

### Test your upgrade
Call the `hey/1` function from the shell you have left open earlier.

```erl
myapp_serv:hey(Pid).
```

And you should get this answer `hey`.

## Conclusion
While this took me some time to figure all that out, I hope this recipe can help Erlang newcomers.

The whole process is a bit cumbersome so I will try to find better or automated ways to do this process. If I find anything, it will most likely be talked about in a different blog post so you might want to have a look at my recent posts.

[1]:https://www.erlang.org/
[2]:http://rebar3.org/
[3]:https://www.erlang.org/doc/reference_manual/code_loading.html#code-replacement
