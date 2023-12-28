---
title: "Making dynamic software updates more comfortable in Erlang/OTP"
date: 2023-12-27T16:27:19+01:00
draft: true
tags: ["Erlang", "OTP", "Rebar3", "DSU", "Github", "Actions", "CI/CD"]
---

## Introduction
My main motivation for this project is to provide a way to test Erlang/OTP releases and dynamic software updates in a continuous integration and delivery pipeline. This will allow developers to use the code replacement functionality that is available in Erlang with more confidence.

### Dynamic software updates
Dynamic software update (DSU) refers to the process of updating parts of a program without halting its execution. It enables running programs to be patched on-the-fly to add features or fix bugs. This capability is particularly crucial for applications that must consistently deliver reliable results. Examples of systems requiring dynamic software include:

- Banking applications
- Air traffic control systems
- Telecommunication systems
- Databases

However, ensuring the correctness of a dynamic software update is challenging and complex. Most of the time, people may discourage its use unless it is strictly necessary. I aim to address this issue by defining methods and providing tools to test the deployment of dynamic software updates.

### Continuous integration
Continuous integration (CI) is a set of techniques used in software engineering that involves verifying that each modification made to the codebase does not include any regressions. By running these tests regularly, typically after each commit, the goal is to detect errors as soon as possible.

### Continuous delivery
Continuous Delivery (CD) typically follows continuous integration and triggers the project build upon successful completion of all tests conducted during continuous integration. In contrast to continuous deployment, continuous integration does not include the deployment of the project.

## The pipeline
This pipeline takes advantage of many components present in [the Dandelion repository](https://github.com/ferd/dandelion). The motivation behind this repository is explained in the post named ["My favorite Erlang Container"](https://ferd.ca/my-favorite-erlang-container.html) on [Fred Hebert's website](https://ferd.ca). 

The different parts of this pipeline will be described below.

### Erlang CI
The first workflow is straightforward and not directly related to dynamic software updates. However, as part of my effort to provide a complete pipeline, this job is essential to ensure proper verification of the modules that are under development.

These verifications include dead code analysis, unit tests, static analysis and coverage analysis. Subsequently, a release is built each time a `push` or a `pull-request` is made to the main branch.

```yml
name: Erlang CI

on:
  push:
    branches: [ "main" ]
  pull_request:
    branches: [ "main" ]

permissions:
  contents: read

jobs:
  build:
    runs-on: ubuntu-latest
    container:
      image: erlang:26.1.2

    steps:
    - uses: actions/checkout@v3
    - name: Run validation checks
      working-directory: erlang
      run: rebar3 check # "Check" is an alias in the rebar.config file
      # {alias, [{check, [xref, dialyzer, ct, {cover, "-v"}]}]}.
    - name: Release can be built
      working-directory: erlang
      run: rebar3 do release, tar # Build the release, then pack it
```

### Relup CI
The second workflow is a little more complicated and is divided in three parts.

```yml
# Beginning of the workflow
name: Relup CI

on:
  pull_request:
    branches: [ "main" ]

permissions:
  contents: read

jobs:
  versions:
    runs-on: ubuntu-latest

    container:
      image: erlang:26.1.2
      options: --user 1001

    steps:
        # Described in the following sections
```

#### Verifying that a previous tag exists
As this workflow is designed to test relups, it does not make sense to execute it if there is no prior release. Furthermore, running this workflow without a preceeding release would result in failure, an outcome that is undesirable. To check the existence of a previous tag, a custom action is employed. It fetches the Github API and verifies the existence of at least one tag. If a tag exists, the return value is set to `"true"`, otherwise it is set to `"false"`. These values will be utilized in the subsequent steps to determine whether they should run or not.

While a git command could have served this purpose, attempts to implement it resulted in errors linked to permissions within the Docker container. This made me opt for a custom action. Altough I Later discovered a resolution for the permission issue, I decided to retain the custom action rather than rewriting this part of the workflow.

```yml
- name: Check if we have tags
    id: check
    uses: ahzed11/has-tags-action@v1.1
```

#### Run validation checks and build releases
This step consists in verifyng that the versions of the applications and the releases have been modified according to the Smoothver naming convention introduced in ["My favorite Erlang Container"](https://ferd.ca/my-favorite-erlang-container.html).

I will brifly describe Smoothver; however, for more details, I recommend reading the post.
In contrast to Semver's MAJOR.MINOR.PATCH, Smoothver uses the RESTART.RELUP.RELOAD format.
- **RESTART**: Indicates a change requiring the server to be restarted, such as updating the ERTS.
- **RELUP**: Indicates a change involving a RELUP, for example, when modifying the state of a worker.
- **RELOAD**: Signifies a change only requiring the reloading of a module.

A sligthly modified version of the script found in the [the Dandelion repository](https://github.com/ferd/dandelion) is used to verify the correctness of the naming and to pack the releases.

The script works as follows:
1. Verify that the version's naming corresponds to the modifications made
2. Create releases
3. Generate an appup using the [rebar3_appup_plugin](https://github.com/lrascao/rebar3_appup_plugin/) by Luis Rascão's
4. Generate a relup
5. Pack the releases

This script enhances the developer's quality of life by automatically detecting when version bumping has been overlooked, automatically generating appups and relups, and packing the releases.

Some developers might express concerns about the automatic generation of the appup file. If someone needs to craft a complex appup file for an upgrade, they can follow their usual approach with the myapp.appup.src file, and the plugin will use it instead of generating it automatically.

```yml
- name: Run validation checks and build releases
    working-directory: erlang
    if: ${{steps.check.outputs.hasTags == 'true'}}
    run: |
        ../scripts/check_versions $(/usr/bin/git log -1 --format='%H') > vsn.log
        cat vsn.log
        cat vsn.log | awk '/Generated appup/ { appup=1 }
                           /relup successfully created!/ { relup=1 }
                           END { if (!appup) { print "appup missing"; exit 1}
                                 if (!relup) { print "relup missing"; exit 1} }'
        OLD=$(cat vsn.log | awk '/OLD:/ {print $2}')
        NEW=$(cat vsn.log | awk '/NEW:/ {print $2}')
        echo "OLD_TAR=$OLD" >> $GITHUB_ENV
        echo "NEW_TAR=$NEW" >> $GITHUB_ENV
```