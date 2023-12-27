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
