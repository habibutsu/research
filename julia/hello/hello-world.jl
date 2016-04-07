
function skynet(from, to)
    if to - from == 1 then
        return from
    end
    println("skynet($from, $to)")
    delta = (to - from) / 10
    sum = 0
    for i in 0:9
        sum = sum + skynet(from + (i*delta), from + (i*delta) + delta)
    end
    return sum
end

function experiment()
    println("My first task")
end

#sum = skynet(0, 100)
#print("result $sum\n")

#@task experiment()
#wait()

t = Task(experiment)
wait(t)