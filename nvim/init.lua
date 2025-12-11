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

-- Recenter Top Bottom setting
local recenter_state = 0
local last_recenter_time = 0
local CONSECUTIVE_THRESHOLD = 2000 -- msec
local function recenter_cycle()
	local current_time = vim.uv.now()
	if current_time - last_recenter_time > CONSECUTIVE_THRESHOLD then
		recenter_state = 0
	end
		
	local positions = {
		{ pos = "center", cmd = "zz" },
		{ pos = "top", cmd = "zt" },
		{ pos = "bottom", cmd = "zb" }
	}

	local current = positions[recenter_state + 1]

	if vim.g.vscode then
		vim.fn.VSCodeExtensionNotify('reveal', current.pos, 0)
	else
		vim.cmd("normal!" .. current.cmd)
	end

	recenter_state = (recenter_state +1) % 3
	last_recenter_time = current_time
end

-- share clipboard between neovim and OS
vim.opt.clipboard:append("unnamedplus")

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	callback = function()
		vim.highlight.on_yank({ timeout = 200 })
	end,
})
	
vim.keymap.set({'n', 'v', 'i'}, '<C-l>', recenter_cycle)
vim.keymap.set('i', 'fd', '<ESC>')
-- emulate Emacs keybindings in insert mode
vim.keymap.set('i', '<C-p>', '<Up>')
vim.keymap.set('i', '<C-n>', '<Down>')
vim.keymap.set('i', '<C-b>', '<Left>')
vim.keymap.set('i', '<C-f>', '<Right>')
vim.keymap.set('i', '<C-a>', '<C-o>^')
vim.keymap.set('i', '<C-e>', '<End>')
vim.keymap.set('i', '<C-d>', '<Del>')
vim.keymap.set('i', '<C-k>', '<C-o>d$')
vim.keymap.set('i', '<C-y>', '<C-r>+')
vim.keymap.set({'i', 'c'}, '<C-h>', '<BS>')
	
vim.keymap.set('v', 'p', '"_dP')

if vim.g.vscode then
	vim.o.timeout = false
	local vscode = require('vscode')
	vim.keymap.set({'n', 'v'}, '<leader>', function()
		vscode.call('whichkey.show')
	end)
else
	vim.keymap.set({'n', 'i'}, '<C-s>', vim.cmd.write)
	vim.keymap.set({'n', 'i'}, '<C-z>', vim.cmd.undo)
	vim.keymap.set({'n', 'v'}, '<leader>h', vim.cmd.nohlsearch)
end
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
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
        		keys = "etovxqpdygfblzhckisuran" } 
		},
		{ "kylechui/nvim-surround",
			version = "^3.0.0",
			evenet = "VeryLazy",
			config = function()
				require("nvim-surround").setup({})
			end
		},
		{ "rapan931/lasterisk.nvim",
			event = "VeryLazy",
			config = function()
				vim.keymap.set('n', '*', function() require("lasterisk").search() end)
			end
		},
	},
	-- Configure any other settings here. See the documentatin for more details.
	
		
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enable = true },
})


