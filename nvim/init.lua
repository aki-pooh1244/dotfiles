-- init.lua
-- 2025/9/5

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup 'mapleader' and 'maplocalleader' before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.encoding = "utf-8"
vim.opt.fileencodings = "utf-8,euc-jp,cp932"
vim.opt.tabstop = 2
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.softtabstop = 4
vim.opt.laststatus = 3
vim.opt.showmatch = true
vim.opt.wrapscan = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.showcmd = true
vim.opt.showmode = true
vim.opt.clipboard = "unnamedplus"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.autoread = true
-- vim.opt.mouse = true

vim.cmd "colorscheme desert"

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		-- add your plugins here
		{ "nvim-mini/mini.nvim", version = false },
    { "smoka7/hop.nvim",
      version = "*",
      config = function()
        require('hop').setup { 
            multi_windows = true,
        }
      end,
      keys = {
          { mode = "", 'f', '<Cmd>HopChar2<CR>' },
          { mode = "", 'F', '<Cmd>HopPattern<CR>' }
      },
      opts = {
        keys = "etovxqpdygfblzhckisuran" } },
	},
	-- Configure any other settings here. See the documentatin for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enable = true },
})


