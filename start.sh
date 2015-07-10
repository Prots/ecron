#!/bin/sh

Name=ecron
start()
{
    erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname ${Name} -config sys -s ecron -noinput -detached && echo $Name started
}

stop()
{
    ps -ef | grep "beam.*$Name" | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null && echo $Name stoped
}

status()
{
    ps -ef | grep "beam.*$Name" | grep -v grep 1>/dev/null && echo $Name started || echo $Name stoped
}

restart() {
    stop
    start
}

case "$1" in
    start)
        $1
        ;;
    stop)
        $1
        ;;
    restart)
        $1
        ;;
    status)
        $1
        ;;
    *)
        echo $"Usage: $0 {start|stop|status|restart}"
        exit 2
esac
exit $?
