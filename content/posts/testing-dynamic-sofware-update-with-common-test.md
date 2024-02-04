---
title: "Testing Erlang/OTP Dynamic Software Updates with Common Test"
date: 2024-02-04T09:35:29+01:00
draft: true
tags: ["Erlang", "OTP", "DSU", "Common Test", "Peer"]
---

## Introduction

In a previous post, the use of Robot Framework to test dynamic software updates was explored. While it worked fine, it is better to stick with tools that are available within Erlang's ecosystem. This is why the use of `Common Test` to test dynamic sofware updates will be explored in this post.

### Dynamic software updates

Dynamic software update (DSU) refers to the process of updating parts of a program without halting its execution. It enables running programs to be patched on-the-fly to add features or fix bugs. This capability is particularly crucial for applications that must consistently deliver reliable results. Examples of systems requiring dynamic software include:

- Banking applications
- Air traffic control systems
- Telecommunication systems
- Databases

However, ensuring the correctness of a dynamic software update is challenging and complex. Most of the time, people may discourage its use unless it is strictly necessary.

### Common Test

> The Common Test framework is a tool that supports implementation and automated execution of test cases to any types of target systems. Common Test is the main tool being used in all testing and verification activities that are part of Erlang/OTP system development and maintenance. - [Common Test Basics](https://www.erlang.org/doc/apps/common_test/basics_chapter)

### Peer

> the peer module provides functions for starting linked Erlang nodes. The node spawning new nodes is called origin, and newly started nodes are peer nodes, or peers [...] The peer node can start on the same or a different host (via ssh) or in a separate container (for example Docker). When the peer starts on the same host as the origin, it inherits the current directory and environment variables from the origin. - [peer](https://www.erlang.org/docs/25/man/peer.html)

## Testing the dynamic software update with Common Test

To test the dynamic software update, a peer node is launched within a docker container containing both the old and the new release. Then, the following operations are applied on the aforementioned node:

1. Starting the old release
2. Modifying its state
3. Upgrading to the new release
4. Testing the state
5. Modifying the state
6. Downgrading to the old release
7. Testing the state

Once these operations are translated into a `Common Test` module, it looks like the following:

```erl
-module(upgrade_downgrade_SUITE).
-behaviour(ct_suite).
-export([all/0, groups/0]).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

groups() ->
    % sequence is used here to make sure that the tests are executed successively
    [{upgrade_downgrade, [sequence], [before_upgrade_case, upgrade_case, after_upgrade_case, before_downgrade_case, downgrade_case, after_downgrade_case]}].

all() ->
    [{group, upgrade_downgrade}].

suite() ->
    [
        {require, old_version}, % 0.1.0
        {require, new_version}, % 0.2.0
        {require, release_name}, % pixelwar
        {require, release_dir} % The directory containing the releases' archives
    ].

init_per_suite(Config) ->
    Docker = os:find_executable("docker"),
    build_image(),
    ReleaseName = ct:get_config(release_name),

    % Use start and not start_link, otherwise the peer is killed when init_per_suite ends
    {ok, Peer, Node} = peer:start(#{name => ReleaseName,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "one", "-i", ReleaseName]}}),

    [{peer, Peer}, {node, Node} | Config].

end_per_suite(Config) ->
    Peer = ?config(peer, Config),
    peer:stop(Peer).

% ========== CASES ==========

before_upgrade_case(Config) ->
    Peer = ?config(peer, Config),

    peer:call(Peer, pixelwar_matrix_serv, set_element, [matrix, {12, 12, 12}]),
    peer:call(Peer, pixelwar_matrix_serv, set_element, [matrix, {222, 222, 222}]),
    
    MatrixAsBin = peer:call(Peer, pixelwar_matrix_serv, get_state, [matrix]),
    ?assertEqual(
        MatrixAsBin,
        <<12:16/little, 12:16/little, 12:16/little, 222:16/little, 222:16/little, 222:16/little>>
    ).

upgrade_case(Config) ->
    Peer = ?config(peer, Config),
    NewVSN = ct:get_config(new_version),
    OldVSN = ct:get_config(old_version),
    ReleaseName = ct:get_config(release_name),
    NewReleaseName = filename:join(NewVSN, ReleaseName),

    {ok, NewVSN} = peer:call(Peer, release_handler, unpack_release, [NewReleaseName]),
    {ok, OldVSN, _} = peer:call(Peer, release_handler, install_release, [NewVSN]),
    ok = peer:call(Peer, release_handler, make_permanent, [NewVSN]),
    
    Releases = peer:call(Peer, release_handler, which_releases, []).

after_upgrade_case(Config) ->
    Peer = ?config(peer, Config),

    MatrixAsBin = peer:call(Peer, pixelwar_matrix_serv, get_state, [matrix]),
    ?assertEqual(
        MatrixAsBin,
        <<12:16/little, 12:16/little, 12:16/little>>
    ).

before_downgrade_case(Config) ->
    Peer = ?config(peer, Config),

    peer:call(Peer, pixelwar_matrix_serv, set_element, [matrix, {13, 13, 13}]),
    
    MatrixAsBin = peer:call(Peer, pixelwar_matrix_serv, get_state, [matrix]),
    ?assertEqual(
        MatrixAsBin,
        <<12:16/little, 12:16/little, 12:16/little, 13:16/little, 13:16/little, 13:16/little>>
    ).

downgrade_case(Config) ->
    Peer = ?config(peer, Config),
    OldVSN = ct:get_config(old_version),

    {ok, OldVSN, _} = peer:call(Peer, release_handler, install_release, [OldVSN]),
    ok = peer:call(Peer, release_handler, make_permanent, [OldVSN]),

    Releases = peer:call(Peer, release_handler, which_releases, []).

after_downgrade_case(Config) ->
    Peer = ?config(peer, Config),

    MatrixAsBin = peer:call(Peer, pixelwar_matrix_serv, get_state, [matrix]),
    ?assertEqual(
        MatrixAsBin,
        <<12:16/little, 12:16/little, 12:16/little, 13:16/little, 13:16/little, 13:16/little>>
    ).

% ========== HELPERS ==========

% Inspired by the peer module's documentation
build_image() ->
    NewVSN = ct:get_config(new_version),
    OldVSN = ct:get_config(old_version),
    ReleaseName = ct:get_config(release_name),
    NewReleaseName = ReleaseName ++ "-" ++ NewVSN,
    OldReleaseName = ReleaseName ++ "-" ++ OldVSN,
    ReleaseDir = ct:get_config(release_dir),

    NewReleasePath = filename:join(ReleaseDir, NewReleaseName ++ ".tar.gz"),
    file:copy(NewReleasePath, "./" ++ NewReleaseName ++ ".tar.gz"),

    OldReleasePath = filename:join(ReleaseDir, OldReleaseName ++ ".tar.gz"),
    file:copy(OldReleasePath, "./" ++ OldReleaseName ++ ".tar.gz"),

    BuildScript = filename:join("./", "Dockerfile"),
    Dockerfile =
        "FROM ubuntu:22.04 as runner\n"
        "EXPOSE 4445\n"
        "WORKDIR /opt/" ++ ReleaseName ++  "\n"
        "COPY [\"" ++ OldReleaseName ++ ".tar.gz\", \"" ++ NewReleaseName ++ ".tar.gz\"" ++ ", \"/tmp/\"]\n"
        "RUN tar -zxvf /tmp/" ++ OldReleaseName ++ ".tar.gz -C /opt/" ++ ReleaseName ++ "\n"
        "RUN mkdir /opt/pixelwar/releases/" ++ NewVSN ++ "\n"
        "RUN cp /tmp/" ++ NewReleaseName ++ ".tar.gz /opt/" ++ ReleaseName ++ "/releases/" ++ NewVSN ++ "/" ++ ReleaseName ++ ".tar.gz\n"
        "ENTRYPOINT [\"/opt/" ++ ReleaseName ++ "/erts-" ++ erlang:system_info(version) ++
        "/bin/dyn_erl\", \"-boot\", \"/opt/" ++ ReleaseName ++ "/releases/" ++ OldVSN ++ "/start\","
        " \"-kernel\", \"inet_dist_listen_min\", \"4445\","
        " \"-erl_epmd_port\", \"4445\","
        " \"-setcookie\", \"secret\"]\n",
    ok = file:write_file(BuildScript, Dockerfile),
    os:cmd("docker build -t " ++ ReleaseName ++ " .").
```

For context, the Erlang/OTP application under consideration is essentially a matrix where pixels can be placed with specific colors.

## Integrating this test within Github Action

At the beginning, the Erlang Docker container was used within the workflow. However it was a mess. First because a merge of the `Docker in Docker` and `Erlang Alpine` images was needed and, also, because it introduced some problem with the Docker `ENTRYPOINT` command that would throw a `no such file or directory` error even though the file was present.

Thankfully the [erlef/setup-beam](https://github.com/erlef/setup-beam) action came to the rescue and allowed me to run Erlang directly on the virtual machine thus removing the need of `Docker in Docker`.

Following is a snippet of the `Github workflow` I wrote:

```yml
name: Relup CI

on:
  pull_request:
    branches: [ "main" ]

permissions:
  contents: read

env:
  RELNAME: pixelwar

jobs:
  versions:
    runs-on: ubuntu-latest

    # Instructions specific to my application
    # [...]
    
    - name: Run relup application
      working-directory: erlang
      run: |
        # Copy the releases built in a previous step into a specific folder
        mkdir relupci
        mkdir relupci/releases/
        cp "${{ env.OLD_TAR }}" relupci/releases/
        cp "${{ env.NEW_TAR }}" relupci/releases/

        # Get the version tags from the archives' names
        OLD_TAG=$(echo "${{ env.OLD_TAR }}"  | sed -nr 's/^.*([0-9]+\.[0-9]+\.[0-9]+)\.tar\.gz$/\1/p')
        NEW_TAG=$(echo "${{ env.NEW_TAR }}"  | sed -nr 's/^.*([0-9]+\.[0-9]+\.[0-9]+)\.tar\.gz$/\1/p')

        # Get the absolute path of relupci/releases/
        RELEASE_DIR=$(readlink -f relupci/releases/)

        # Write the configuration file required by the CT Suite
        echo -e "{old_version, \"$OLD_TAG\"}.\n{new_version, \"$NEW_TAG\"}.\n{release_name, \"$RELNAME\"}.\n{release_dir, \"$RELEASE_DIR\"}." >> ./test/config.config

        # Create a directory for the results and launch the CT Suite
        mkdir results
        rebar3 ct --dir . --verbose true --config ./config.config --logdir ./results --label relup-ci

    - name: Upload test results
      uses: actions/upload-artifact@v3
      if: always()
      with:
        name: results
        path: ./erlang/test/results
        retention-days: 1

```

For now, the results are simply linked to the workflow's run but it is easy to imagine uploading the results'html page to `Github Pages` and posting a link to it in the corresponding pull request.

## Conclusion

While all of this might seem straightforward, it took me quite some time to come to this point.

However, I am happy that I managed to make it work because it opens new possiblities such as writing complex tests for `DSUs` which should improve the developer's confidence and also integrating the Erlang Ecosystem into Github.
