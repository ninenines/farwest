PROJECT = farwest
PROJECT_DESCRIPTION = Framework for building RESTful HATEOAS-driven applications
PROJECT_VERSION = 0.1.0

DEPS = cowboy gun
BUILD_DEPS = cowlib
dep_cowboy_commit = master
dep_gun_commit = master
dep_cowlib_commit = master

TEST_DEPS = ct_helper farwest_demo
dep_ct_helper = git https://github.com/ninenines/ct_helper master
dep_farwest_demo = git https://github.com/ninenines/farwest_demo master

export LOCAL_FARWEST=1

include erlang.mk
