---
title: "Publishing Common Test Results on Github"
date: 2024-03-11T14:30:31+01:00
draft: false
tags: ["Erlang", "OTP", "Github", "Common Test", "CI", "CD", "Actions"]
summary: Discover how the publish unit test result action can be used with the common test module
cover:
  image: "images/common-test-publish-unit-test-result-on-github.png"
  caption: "Captured from https://github.com/Ahzed11/pixelwar"
---

## Introduction

Running your tests in Github and reading the logs to discover what went wrong works fine but it can be quite tedious.

This is why I wanted to find a way to summarize the tests that run in my workflows. After some research, I stumbled upon Github Actions that are specifically crafted for this purpose.

In this post, I will introduce you to the action I currently use in my repositories.

## Publish Unit Test Result Action

This is the name of the action I use. Many Github actions exist for this purpose but I decided to use [this one](https://github.com/EnricoMi/publish-unit-test-result-action) because it is used in the Erlang/OTP repository.

However, it does not work out of the box with `common test` because `common test` outputs its reports in `HTML` and this action requires `JSON` or `XML`. In the next section, we will see how to remedy to this problem.

Using this action is rather simple. Assuming that the common test result are located under `./results`, you only have to add this step to your workflow:

```yml
- name: Erlang CI results
  uses: EnricoMi/publish-unit-test-result-action@v2
  if: always()
  with:
    check_name: Erlang CI results
    files: |
      ./results/**/*.xml
```

However, two important things to note are:

1. If you use this action in multiple workflows, you have to give a unique name in the `check_name` option
1. You must modify the permissions of your workflow. [Here](https://github.com/EnricoMi/publish-unit-test-result-action?tab=readme-ov-file#permissions) is a link explaining how to modify your permissions.

## Output XML reports with common test

To output the reports generated by `common test` in `XML`, we will use the `common test hook` functionality.

> The Common Test Hook (CTH) framework allows extensions of the default behavior of Common Test using hooks before and after all test suite calls. CTHs allow advanced Common Test users to abstract out behavior that is common to multiple test suites without littering all test suites with library calls. This can be used for logging, starting, and monitoring external systems, building C files needed by the tests, and so on. [Common Test Hooks](https://www.erlang.org/doc/apps/common_test/ct_hooks_chapter#general)

### Add cth_surefire to your common test hooks

The `Common Test Hook` that we will use is called `cth_surefire`.

> cth_surefire captures all test results and outputs them as surefire XML into a file. The created file is by default called junit_report.xml. [Common Test Hooks](https://www.erlang.org/doc/apps/common_test/ct_hooks_chapter#built-in-cths)

One thing to note here, if you use Jenkins instead of Github actions, is that the `cth_surefire` format can be used directly in `Jenkins` to display test results.

#### Using the ct_run bash command

If you launch `common test` directly from the command line, you can add the `ct_hooks` parameter followed by `cth_surefire`

```sh
ct_run -dir . -logdir ./results -ct_hooks cth_surefire
```

#### Using rebar3

If you use rebar3, it is as simple as adding this line to your rebar.config

```erlang
{ct_opts, [{ct_hooks, [cth_surefire]}]}.
```

## Conclusion

In this post, we looked at how to convert common test results into XML format and connect them with GitHub Actions, which were initially made for different frameworks. By using the publish-unit-test-result-action and the cth_surefire hook in common test, we've simplified the process of summarizing test results.

I hope this can be useful for your own projects.
