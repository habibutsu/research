use "collections"

actor SkyNet

  var _count: U32 = 0
  var _sum: U64 = 0

  var _env: Env

  new create(env: Env) =>
    _env = env

  be start(receiver: Main) =>
    SkyNet(_env).skynet(this, 0, 100000)
    this.wait_main_result(receiver, 1)

  be skynet(receiver: SkyNet, from: U64, to: U64) =>
    if (to - from) == 1 then
      //_env.out.print("result " + from.string())
      receiver.set_result(from)
      return
    end
    let delta: U64 = (to - from) / 10

    for i in Range[U64](0, 10) do
      let nfrom: U64 = from + (i*delta)
      let nto: U64 = from + (i*delta) + delta
      //_env.out.print("from " + nfrom.string() + " to "+ nto.string())
      SkyNet(_env).skynet(this, nfrom, nto)
    end
    this.wait_result(receiver, 10)

  be set_result(num: U64) =>
    _sum = _sum + num
    _count = _count + 1

  be wait_result(receiver: SkyNet, count: U32) =>
    if _count == count then
      receiver.set_result(_sum)
      return
    end
    this.wait_result(receiver, count)

  be wait_main_result(receiver: Main, count: U32) =>
    if _count == count then
      receiver.result(_sum)
      return
    end
    this.wait_main_result(receiver, count)


actor Main

  var _env: Env

  new create(env: Env) =>
    _env = env
    env.out.print("Hello, world! ")
    SkyNet(env).start(this)

  be result(num: U64) =>
    _env.out.print("Result " + num.string())