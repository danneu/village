'use strict'

require('spectre.css')
require('./css/index.scss')

const Elm = require('./Main.elm')
const mountNode = document.getElementById('main')

const app = Elm.Main.embed(mountNode)
