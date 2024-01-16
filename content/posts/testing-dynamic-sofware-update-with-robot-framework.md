---
title: "Testing Erlang/OTP Dynamic Software Updates with Robot Framework"
date: 2024-01-14T10:56:27+01:00
draft: false
tags: ["Erlang", "OTP", "DSU", "Robot Framework"]
---

## Introduction

### Dynamic software updates

Dynamic software update (DSU) refers to the process of updating parts of a program without halting its execution. It enables running programs to be patched on-the-fly to add features or fix bugs. This capability is particularly crucial for applications that must consistently deliver reliable results. Examples of systems requiring dynamic software include:

- Banking applications
- Air traffic control systems
- Telecommunication systems
- Databases

However, ensuring the correctness of a dynamic software update is challenging and complex. Most of the time, people may discourage its use unless it is strictly necessary.

### Robot Framework

> Robot Framework is a generic open source automation framework. It can be used for test automation and robotic process automation (RPA) - [Robot Framework](https://robotframework.org/)


This automation framework has been selected after coming across it during my internship. It is open-source, is easily expandable with Python, has an easy syntax and comes with a diverse range of pre-existing libraries and tools.

Following is what a Robot Framework test suite might look like:
```robot
*** Settings ***
Documentation     A test suite for valid login.
...
...               Keywords are imported from the resource file
Resource          keywords.resource
Default Tags      positive

*** Test Cases ***
Login User with Password
    Connect to Server
    Login User            ironman    1234567890
    Verify Valid Login    Tony Stark
    [Teardown]    Close Server Connection

Denied Login with Wrong Password
    [Tags]    negative
    Connect to Server
    Run Keyword And Expect Error    *Invalid Password    Login User    ironman    123
    Verify Unauthorized Access
    [Teardown]    Close Server Connection
```

## Interfacing between Robot Framework and an OTP release

It is possible to interact with an OTP release through various commands that can be called via bash. These commands include, for example, starting the release, stopping it, sending RPCs, etc...

This means that it is also possible to interact with the release via Robot Framework using the python subprocess library.
Below is a short library written to leverage this functionality.

```py
# otp.py


import subprocess
from robot.api.deco import not_keyword
from robot.libraries.BuiltIn import BuiltIn

@not_keyword
def run(command):
    result = subprocess.run(command, shell = True, executable="/bin/bash", capture_output=True)
    stdout = result.stdout.decode("utf-8")
    return stdout

def start_release():
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    START_COMMAND = f"MATRIX_WIDTH=128 MATRIX_HEIGHT=128 {RELEASE_PATH} daemon"
    return run(START_COMMAND)

def stop_release():
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    START_COMMAND = f"{RELEASE_PATH} stop"
    return run(START_COMMAND)

def send_rpc(module, function, arguments):
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    SEND_RPC_COMMAND = f"{RELEASE_PATH} rpc {module} {function} {arguments}"
    return run(SEND_RPC_COMMAND)

def upgrade_release(version):
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    UPGRADE_COMMAND = f"{RELEASE_PATH} upgrade {version}"
    stdout = run(UPGRADE_COMMAND)

    if "release_package_not_found" in stdout:
        raise AssertionError(f"Command failed: {stdout}")

def downgrade_release(version):
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    DOWNGRADE_COMMAND = f"{RELEASE_PATH} downgrade {version}"
    stdout = run(DOWNGRADE_COMMAND)

    if "release_package_not_found" in stdout:
        raise AssertionError(f"Command failed: {stdout}")

def should_be_equal_as_erlang_bytes(actual, expected, msg=None):
    from_as_str = actual.replace("#Bin<", "")
    from_as_str = from_as_str.replace(">\n", "")
    
    if from_as_str != expected:
        if msg:
            raise AssertionError(f"Values are not equal: {from_as_str} != {expected}. {msg}")
        else:
            raise AssertionError(f"Values are not equal: {from_as_str} != {expected}")

```

## Writing an application-specific library

The preceding code snippet is intentionally generic, serving as a common foundation for all tests. However, relying solely on a generic library may not be convenient. This is why an app-specific library becomes necessary.

For context, the Erlang/OTP application under consideration is essentially a matrix where pixels can be placed with specific colors. The following is the library written specifically for this app, leveraging functions from the previous library.

```py
# matrix.py


from otp import send_rpc

MODULE = "pixelwar_matrix_serv"
PROCESS = "matrix"

def set_pixel(x, y, color):
    return send_rpc(MODULE, "set_element", f"[{PROCESS}, {{ {x}, {y}, {color} }}]")

def get_matrix_state():
    return send_rpc(MODULE, "get_state", f"[{PROCESS}]")
```

## Testing the dynamic software update with Robot Framework

With the generic and specific libraries prepared, we can now compose the Robot Framework test. The test for the dynamic software update of this application involves the following operations:

1. Starting the old release
2. Modifying its state
3. Upgrading to the new release
4. Testing the state
5. Modifying the state
6. Downgrading to the old release
7. Testing the state

When translated into a Robot Framework test suite, it looks like the following:

```robot
*** Variables ***
${OLD_VERSION}    0.1.0
${NEW_VERSION}    0.2.0
${RELEASE_PATH}    erlang/_build/default/rel/pixelwar/bin/pixelwar

*** Settings ***
Suite Setup     Start Release
Suite Teardown     Stop Release
Library    matrix.py
Library    otp.py

*** Test Cases ***
Setup state before upgrade
    Set Pixel    12    12    12
    Set Pixel    222    222    222
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,222,0,222,0,222,0
    
upgrade release
    Upgrade Release    ${NEW_VERSION}

Test state after upgrade
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0

Setup state before downgrade
    Set Pixel    13    13    13
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,13,0,13,0,13,0

downgrade release
    Downgrade Release    ${OLD_VERSION}

Test state after downgrade
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,13,0,13,0,13,0

```

This test suite can be launched with the following command:

```bash
robot -v OLD_VERSION:${OLD_TAG} -v NEW_VERSION:${NEW_TAG} -v RELEASE_PATH:${RELEASE_PATH} ./test/my_test.robot
```

## Conclusion

While there are possibilities for enhancements in the generic library, such as improving error reporting, this example already showcases the potential of Robot Framework in the context of Erlang/OTP dynamic software updates.

The ease of creating Python libraries, coupled with the readability and maintainability of tests within Robot Framework, positions it as a good choice for testing dynamic software updates.
