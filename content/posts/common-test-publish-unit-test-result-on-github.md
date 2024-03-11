---
title: "Publish Common Test Result on Github"
date: 2024-03-11T14:30:31+01:00
draft: true
tags: ["Erlang", "OTP", "Github", "Common Test", "CI", "CD", "Actions"]
---

## Add cth_surefire to your common test hooks

> cth_surefire captures all test results and outputs them as surefire XML into a file. The created file is by default called junit_report.xml. [common test](https://www.erlang.org/doc/apps/common_test/ct_hooks_chapter#built-in-cths)

### Using the ct_run bash command

If you launch common tests directly from the command line, you can add the `ct_hooks` parameter followed by `cth_surefire`

```sh
ct_run -dir . -ct_hooks cth_surefire
```

### Using rebar3

If you use rebar3, it is as simple as adding this line to your rebar.config

```erlang
{ct_opts, [{ct_hooks, [cth_surefire]}]}.
```

## Add the publish unit test result action

Many Github actions exist for this purpose but I decided to use this one because it is the one that is present in the Erlang/OTP repository.

Using it is rather simple. Assuming that the common test result are located under `./results`, you only have to add this step to your workflow:

```yml
- name: Erlang CI results
  uses: EnricoMi/publish-unit-test-result-action@v2
  if: always()
  with:
    check_name: Erlang CI results
    files: |
      ./results/**/*.xml
```

Two important things to note are:

1. If you use this action in multiple workflows, you have to give a unique name in the `check_name` option
1. You have to modify the permissions of your workflow. [Here](https://github.com/EnricoMi/publish-unit-test-result-action?tab=readme-ov-file#permissions) is a link explaining how to modify your permissions.
