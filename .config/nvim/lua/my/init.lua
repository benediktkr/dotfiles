require 'my.config'
require('lazy').setup('my.plugins', {
  change_detection = {
    notify = false,
  },
})
