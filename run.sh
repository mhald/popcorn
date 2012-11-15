./rebar skip_deps=true compile;
erlc -o apps/popcorn/ebin/ 

CONFIG=rel/files/sys.config

echo "Config is" $CONFIG

erl -pa apps/*/ebin -pa deps/*/ebin -smp enable \
 	-boot start_sasl -s lager -s ssl -s popcorn \
	-config $CONFIG -args_file rel/files/vm.args

