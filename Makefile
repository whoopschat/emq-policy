PROJECT = emq_policy_server
PROJECT_DESCRIPTION = EMQ Policy Server
PROJECT_VERSION = 1.0.0

BUILD_DEPS = emqttd cuttlefish ibrowse

dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse

NO_AUTOPATCH = cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_policy.conf -i priv/emq_policy.schema -d data
