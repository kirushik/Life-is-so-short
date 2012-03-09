#!/bin/sh
erl -compile life
erl -noinput -s life run #-s init stop
