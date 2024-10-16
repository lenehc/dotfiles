return {
  'MeanderingProgrammer/render-markdown.nvim',
  dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
  -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
  -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
  ---@module 'render-markdown'
  ---@type render.md.UserConfig
  opts = {},
  config = {
    heading = {
      enabled = true,
      sign = false,
      position = 'overlay',
      icons = { '# ', '  ## ', '   ### ', '    #### ', '     ##### ', '      ###### ' },
      width = 'full',
      left_margin = 0,
      left_pad = 0,
      right_pad = 0,
      min_width = 0,
      backgrounds = {},
      foregrounds = {
        'RenderMarkdownH1',
      },
    },
    bullet = {
      enabled = true,
      icons = { '-', ' - ', ' - ', ' - ' },
      left_pad = 1,
      right_pad = 1,
      highlight = 'RenderMarkdownBullet',
    },
  },
}
