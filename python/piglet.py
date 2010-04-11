import pyglet

# Load an image
image = pyglet.resource.image('marilyn.jpg')

# Play some music
music = pyglet.resource.media('cartman.mp3')
music.play()

# Get display information
platform = pyglet.window.get_platform()
display = platform.get_default_display()
print "Display: ", display

# Show all screens
for screen in display.get_screens():
  print screen

# Setup OpenGL configuration
screen = display.get_default_screen()
template = pyglet.gl.Config(alpha_size = 8)
config = None

try:
  config = screen.get_best_config(template)
except pyglet.window.NoSuchConfigException:
  print "Fail. Reverting to default configuration"
  template = pyglet.gl.Config()
  config = screen.get_best_config(template)

# Setup OpenGL context
context = config.create_context(None)

# New window
# window = pyglet.window.Window(config=config)
window = pyglet.window.Window()

# Draw some text
label = pyglet.text.Label("Hello Marilyn!",
                          font_name = "Times New Roman",
                          font_size = 36,
                          x = window.width // 2, y = window.height // 2,
                          anchor_x = 'center', anchor_y = 'center')

@window.event
def on_draw():
  window.clear()
  image.blit(0, 0)
  label.draw()

@window.event
def on_key_press(symbol, modifiers):
  print 'boo ', symbol, ' ', modifiers

@window.event
def on_mouse_press(x, y, button, modifiers):
  print 'squeak ', x, ':', y, ' - ', button, ' ', modifiers

pyglet.app.run()
