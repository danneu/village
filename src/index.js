'use strict'

require('./css/index.scss')

const Elm = require('./Main.elm')
const mountNode = document.getElementById('main')

const app = Elm.Main.embed(mountNode)

// CONSTANTS

const tileSize = 10
const canvasWidth = 1000
const canvasHeight = 50
const rowCount = Math.floor(canvasHeight / tileSize)
const colCount = Math.floor(canvasWidth / tileSize)

// DRAWING

const canvas = document.querySelector('canvas')
canvas.width = canvasWidth
canvas.height = canvasHeight
window.canvas = canvas
const ctx = canvas.getContext('2d')

function drawVillager ({action}) {
  // Only draw villager if they are moving
  if (action.kind !== 'Moving') return
  const [x, y] = action.position
  ctx.fillStyle = 'rgb(0, 0, 0)'
  ctx.fillRect(x * tileSize, y * tileSize, tileSize, tileSize)
}

function clearCanvas () {
  ctx.clearRect(0, 0, canvasWidth, canvasHeight)
}

function drawGrid () {
  ctx.strokeStyle = '#000000'
  // Draw cols
  for (let x = tileSize; x < canvasWidth; x += tileSize) {
    ctx.beginPath()
    ctx.moveTo(x, 0)
    ctx.lineTo(x, canvasHeight)
    ctx.stroke()
  }
  // Draw rows
  for (let y = tileSize; y < canvasHeight; y += tileSize) {
    ctx.beginPath()
    ctx.moveTo(0, y)
    ctx.lineTo(canvasWidth, y)
    ctx.stroke()
  }
}

app.ports.draw.subscribe(state => {
  clearCanvas()
  drawGrid()
  state.villagers.forEach(drawVillager)
})
