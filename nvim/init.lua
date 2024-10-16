vim.g.mapleader = " "

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	vim.fn.system({ 
		"git", 
		"clone", 
		"--filter=blob:none", 
		"--branch=stable", 
		"https://github.com/folke/lazy.nvim.git", 
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("plugins")

vim.cmd("colorscheme gruvbox")

vim.keymap.set('n', ';', ':')
vim.opt.mouse="a"
vim.opt.clipboard = 'unnamedplus'
vim.opt.expandtab = true
vim.opt.number = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
