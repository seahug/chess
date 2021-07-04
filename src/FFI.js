'use strict'

// helpers

const broadcastEvent = messageTarget => event => {
  const eventTarget = document.querySelector('#' + messageTarget)
  if (eventTarget) {
    eventTarget.dispatchEvent(customEvent(event))
  }
  else {
    logError(`${domQuery} undefined, cannot dispatch event: `, event)
  }
}

const customEvent = detail =>
  new CustomEvent('purescript', { detail })

const log = level => (...args) => console[level]('FFI: ', ...args)

const logInfo = (...args) => log('log')(...args)

const logError = (...args) => log('error')(...args)

// exports

exports.registerServiceWorker = () => {
  if ('serviceWorker' in navigator) {
    const onLoad = () =>
      navigator.serviceWorker
        .register('sw.js')
        .then(({ scope }) =>
          logInfo(
            'ServiceWorker registration successful with scope: ',
            scope
          )
        )
        .catch(error =>
          logInfo('ServiceWorker registration failed: ', error)
        )
    window.addEventListener('load', onLoad)
  } else {
    logInfo('ServiceWorker unsupported')
  }
}

exports.copyToClipboard = id => () => {
  const element = document.getElementById(id)

  element.select()

  element.setSelectionRange(0, 99999)

  document.execCommand('copy')
}

exports.detail = ({ detail }) => detail

exports.makeBugoutFFI = tuple =>
  left =>
  right =>
  connections =>
  seen =>
  roomCode =>
  remoteMessageTarget =>
  localMessageTarget =>
  callback =>
  () => {
  var fresh = true
  const installBugoutHandlers = bugout => {
    bugout.on("connections", count => {
      if (count == 0 && fresh) {
        fresh = false
        callback(right(bugout))()
      }
      broadcastEvent(localMessageTarget)(connections(count))
    })

    bugout.on("message", (address, message) => {
      logInfo("incoming message length: ", message.length)
      address === bugout.address()
        || broadcastEvent(remoteMessageTarget)(message)
    })

    bugout.on("seen", address => {
      broadcastEvent(localMessageTarget)(seen(address))
    })

    bugout.on("ping", address => {
      if (address !== bugout.address()) {
        logInfo("ping from: ", address)
      } else {
        logInfo("self ping? ", address)
      }
    })

    bugout.on("timeout", address => {
      if (address !== bugout.address()) {
        logInfo("timeout: ", address)
      } else {
        logInfo("self timeout? ", address)
      }
    })
  }

  const options = {
    // try these options again when we can do something about timeouts
    // heartbeat: 5000,
    // timeout: 10000,
    announce: [
      "wss://Chess-p2p-tracker.herokuapp.com",
      "wss://hub.bugout.link",
      "wss://tracker.openwebtorrent.com",
      "wss://tracker.btorrent.xyz"
    ]
  }

  try {
    const bugout = new Bugout(roomCode, options)
    installBugoutHandlers(bugout)
  } catch (error) {
    callback(left(error))()
  }
}

exports.address = bugout => () => bugout.address()

exports.send = bugout => message => () => {
  logInfo("sent message length: ", message.length)
  bugout.send(message)
}

// https://stackoverflow.com/a/8809472
exports.genUuid = () => { // Public Domain/MIT
  var d = new Date().getTime()

  // Time in microseconds since page-load or 0 if unsupported
  var d2 = (performance && performance.now && (performance.now() * 1000)) || 0
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
    .replace(/[xy]/g, c => {
      // random number between 0 and 16
      var r = Math.random() * 16

      if(d > 0){
        // Use timestamp until depleted
        r = (d + r) % 16 | 0
        d = Math.floor(d / 16)
      } else {
        // Use microseconds since page-load if supported
        r = (d2 + r) % 16 | 0
        d2 = Math.floor(d2 / 16)
      }
      return (c === 'x' ? r : (r & 0x3 | 0x8))
        .toString(16)
    })
}

exports.arrayBufferAsString = buffer =>
  String.fromCharCode.apply(null, new Uint8Array(buffer))

exports.stringAsArrayBuffer = string => {
  const stringLength = string.length
  const buffer = new ArrayBuffer(stringLength * 2)
  const bufferView = new Uint8Array(buffer)
  for (let i = 0; i < stringLength; i++) {
    bufferView[i] = string.charCodeAt(i)
  }
  return buffer
}

// cannot eta-reduce this function because LZString does not
// exist in our test suite - we currently import the library
// in an HTML script tag
exports.compressString = s => LZString.compress(s)

exports.decompressStringFFI = just => nothing => s => {
  const result = LZString.decompress(s)
  return result == null
    ? nothing
    : just(result)
}

