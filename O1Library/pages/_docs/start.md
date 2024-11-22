---
title: Introduction
---

# Introduction to O1Library

__O1Library__ is a toolkit designed for the course [Programming 1 (a.k.a. O1)](https://plus.cs.aalto.fi/o1/) 
at Aalto University. It contains an assortment of tools; most prominently, it provides a 
framework for simple graphical programming and utilities for playing sound.

## Using O1Library as a Student

This is the “front page” of O1Library’s documentation. However, this is probably not the best
place to start learning about O1Library as a student. That’s because the library’s most relevant 
content is introduced bit by bit in the chapters of O1’s custom ebook alongside the associated 
programming concepts and assignments.

You may nevertheless find this documentation useful as a reference. You can also find some
optional content here that you may wish to try.

O1Library’s main package is called simply `o1`. The contents of that package are available 
with the simple command `import o1.*` in your Scala programs. Some of them you’ll use a lot; 
some of them you won’t necessarily need at all. For a list of what is available there, see 
the docs for [[that package|o1]].

## Subpackages

The tools available in the top-level package `o1` are actually implemented in a number 
of subpackages (`o1.gui`, `o1.sound`, etc.). The top-level package contains “shortcut 
aliases” to those actual implementations. The aliases are there to enable the convenient 
`import o1.*` command. 

The key subpackages are listed below. They contain some additional tools beyond what
is available via `import o1.*`.

- [[o1.gui]] contains tools for building simple GUIs. The toolkit 
  is particularly well suited to constructing GUIs that display 
  information as 2D images and/or geometric shapes. It is not
  designed for demanding graphical needs that call for high 
  efficiency. Some of these tools are built on the Swing GUI 
  library, and the two libraries can be used in combination. 

- `o1.sound` consists of two subpackages. One, [[o1.sound.midi]], 
  handles MIDI sound described in terms of notes, instruments, 
  and other directives. Another, [[o1.sound.sampled]], is for 
  working with recorded sound samples. 

- [[o1.world]] contains tools for locations and movement in 
  two-dimensional space. The subpackage [[o1.world.objects]] 
  contains additional tools for representing entities that 
  reside within two-dimensional spaces.

- [[o1.grid]] contains tools for working with grid-like
  two-dimensional structures. 

- [[o1.util]] contains miscellaneous tools for added 
  convenience. They are used internally by some given 
  programs in O1. The package is not directly relevant 
  to O1 students. Not all of its contents are listed in 
  this documentation.
