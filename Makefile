PROJECT = emq_policy_server
PROJECT_DESCRIPTION = policy server for EMQ
PROJECT_VERSION = 1.0.0

BUILD_DEPS = emqttd cuttlefish

dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_policy.conf -i priv/emq_policy.schema -d data