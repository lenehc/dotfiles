return {
  "epwalsh/obsidian.nvim",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    workspaces = {
      {
        name = "notes",
        path = "~/notes",
      }
    },
    daily_notes = {
      folder = "journal",
      date_format = "%Y-%m-%d",
      alias_format = nil,
      default_tags = {},
      template = nil
    },
  },
}
