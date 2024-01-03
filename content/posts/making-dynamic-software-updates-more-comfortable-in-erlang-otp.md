---
title: "Making dynamic software updates more comfortable in Erlang/OTP thanks to CI/CD"
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

These verifications include dead code analysis, unit tests, static analysis, coverage analysis... Subsequently, a release is built each time a `push` or a `pull-request` is made to the main branch to verify that everything compiles.

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

The second workflow is a little more complicated and is divided in three parts. It runs on every pull request made to main.

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
      options: --user 1001 # Fix for the user permissions

    steps:
        # Described in the following sections
```

#### Verifying that a previous tag exists

As this workflow is designed to test relups, it does not make sense to execute it if there is no prior release. Furthermore, running this workflow without a preceeding release would result in failure, an outcome that is undesirable. To check the existence of a previous tag, a custom Github action is employed. It fetches the Github API and verifies the existence of at least one tag. If a tag exists, the return value is set to `"true"`, otherwise it is set to `"false"`. These values will be utilized in the subsequent steps to determine whether they should be executed or not.

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

#### Run the update

This step is the one that is the most related to dynamic software updates. It consists in running and validating the update of the release through a series of operations. The sequence is structured as follows:

1. Start the old release
2. Modify its state
3. Upgrade to the new release
4. Test the state
5. Modify the state
6. Downgrade to the old release
7. Test the state

Splitting these tests and state modifications into four different bash scripts allows us to set an arbitrarily complex state up and to test it at every important stage. To interact with the application, remote procedure calls are used.

Here is an exemple:

```sh
#!/bin/bash

binary1=$(./relupci/bin/pixelwar rpc pixelwar_matrix_serv get_state [matrix])
binary2='#Bin<12,0,12,0,12,0>'
echo $binary1
echo $binary2

if [[ $binary1 == $binary2 ]]; then
  echo "Success"
  exit 0
else
  echo "Fail"
  exit 1
fi
```

```yml
- name: Run relup application
    working-directory: erlang
    if: ${{steps.check.outputs.hasTags == 'true'}}
    run: |
        mkdir relupci
        tar -xvf "${{ env.OLD_TAR }}" -C relupci
        MATRIX_WIDTH=128 MATRIX_HEIGHT=128 relupci/bin/pixelwar daemon #1
        cp "${{ env.NEW_TAR }}" relupci/releases/


        OLD_TAG=$(echo "${{ env.OLD_TAR }}"  | sed -nr 's/^.*([0-9]+\.[0-9]+\.[0-9]+)\.tar\.gz$/\1/p')
        NEW_TAG=$(echo "${{ env.NEW_TAR }}"  | sed -nr 's/^.*([0-9]+\.[0-9]+\.[0-9]+)\.tar\.gz$/\1/p')
        
        echo "Launch before upgrade test" 
        ./test/before_upgrade.sh #2

        relupci/bin/pixelwar upgrade ${NEW_TAG} #3
        relupci/bin/pixelwar versions

        echo "Launch after upgrade test"
        ./test/after_upgrade.sh #4

        echo "Launch before downgrade test"
        ./test/before_downgrade.sh #5

        relupci/bin/pixelwar downgrade ${OLD_TAG} #6

        echo "Launch after downgrade test"
        ./test/after_downgrade.sh #7
```

### Publish tarball

In the last workflow, the update is bundled and uploaded to its corresponding Github release. This workflow is triggered for every pushed tag.

This workflow is an adaptation of the one found in the [the Dandelion repository](https://github.com/ferd/dandelion). However, instead of working with S3 like the original, it is customized to operate smoothly within GitHub.

```yml
name: Publish tarball

on:
  push:
    tags: [ "v[0-9]+.[0-9]+.[0-9]+" ]

env:
  RELNAME: pixelwar

jobs:
# ...
```

#### Fetch version

In this step, the system retrieves the most recent release linked to the current repository and its tag. If it is an upgrade, an environment variable is then configured accordingly. A custom action is used so we do not need to provide a Token to fetch the latest release.

```yml
# ...
build:
  name: Prepare build artifacts
  runs-on: ubuntu-latest

  container:
    image: erlang:26.1.2
    options: --user 1001

  steps:
    - name: Get latest release
        id: get-latest-release
        uses: ahzed11/get-latest-release-action@v1.2
        with:
          keepv: false # Remove the v in front of the version
    
    - uses: actions/checkout@v3
      with:
        fetch-depth: 0
    - name: Fetch manifest version
        run: |
          NEW_VSN=${GITHUB_REF##*/v}
          echo "VSN=$NEW_VSN" >> $GITHUB_ENV
          
          OLD_VSN=${{steps.get-latest-release.outputs.release}}
          echo "OLD_VSN=$OLD_VSN" >> $GITHUB_ENV

          IS_UPGRADE=$(echo "$NEW_VSN $OLD_VSN" | awk -vFS='[. ]' '($1==$4 && $2>$5) || ($1==$4 && $2>=$5 && $3>$6) {print 1; exit} {print 0}')
          if [ "$IS_UPGRADE" -eq 1 ]; then
              echo "RELUP=1" >> $GITHUB_ENV
          else
              echo "RELUP=0" >> $GITHUB_ENV
          fi
    - run: |
            echo "${{ env.OLD_VSN }} -> ${{ env.VSN }} : ${{ env.RELUP }}"
#...
```

#### Build a tarball

In this step, a tarball with the release is built. If it is an upgrade, an appup and a relup are also generated.

```yml
# ...
- name: Build a tarball
  working-directory: erlang
  run: |
    if [ ${{ env.RELUP }} -eq 1 ]; then
      ORIG=$(/usr/bin/git log -1 --format='%H')
      git checkout v${{ env.OLD_VSN }}
      rebar3 do clean -a, release
      git checkout $ORIG
      rebar3 do clean -a, release
      rebar3 appup generate
      rebar3 relup -n ${{ env.RELNAME }} -v ${{ env.VSN }} -u ${{ env.OLD_VSN }}
    else
      rebar3 release
    fi
    rebar3 tar
    BUILD=$(ls _build/default/rel/${{ env.RELNAME }}/${{ env.RELNAME }}-*.tar.gz)
    mkdir ../_artifacts
    cp $BUILD ../_artifacts/${{ env.RELNAME }}-${{ env.VSN }}.tar.gz
# ...
```

#### Upload build artifacts

Github actions allows us to store the results of the preceding steps. This feature is utilizated to retrieve these artifacts in the succeeding job.

```yml
#...
- name: Upload build artifacts
  uses: actions/upload-artifact@v3
  with:
    name: artifacts
    path: _artifacts
    retention-days: 1
#...
```

#### Publish build artifacts

In the final step, which is a distinct job, the objective is to create a release and attach it the build artifacts. To achieve this, the [release-action](https://github.com/ncipollo/release-action) made by [ncipollo](https://github.com/ncipollo) is used.

```yml
# ...
upload:
  name: Publish build artifacts
  needs: build
  runs-on: ubuntu-latest

  permissions:
    id-token: write
    contents: write # Necessary to upload files to the release

  steps:
    - uses: actions/checkout@v3
    - name: Get build artifacts
      uses: actions/download-artifact@v3
      with:
        name: artifacts
        path: _artifacts # Output folder from the previous job

    - name:  Upload release
      uses: ncipollo/release-action@v1
      with:
        artifacts: |
          _artifacts/*.tar.gz
```

## Possible improvements

- Continuous deployment
- Usage of `escript` instead of `bash`
- Sync modules' version with the app's version so it is easy to know which version to use in `code_change`
