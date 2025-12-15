
vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = 'main' },
  "https://github.com/nvim-treesitter/nvim-treesitter-context",
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/whonore/Coqtail",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/voldikss/vim-floaterm",
  "https://github.com/jremmen/vim-ripgrep"
})

local fzfcmd='rg --fixed-strings --line-number --color=always --column'
local fzfopts=string.gsub('--bind "change:reload:$cmd {q}"', "%$(%w+)",fzfcmd)

vim.env.FZF_DEFAULT_COMMAND=fzfcmd 
vim.env.FZF_DEFAULT_OPTS=fzfopts

vim.lsp.config('coq-lsp', { cmd = 'coq-lsp', 
  cmd = { 'coq-lsp' },
  filetypes = { 'coq' },
  root_markers = { '_CoqProject', '.git' },
}) 
vim.lsp.enable('hls')  
vim.lsp.enable('coq-lsp')

vim.diagnostic.enable = true
vim.diagnostic.config({
  underline = true,
  signs = false,
})

vim.api.nvim_create_user_command('FloatermFiles', function()
 vim.fn.setenv("FZF_DEFAULT_COMMAND", nil)
 vim.fn.setenv("FZF_DEFAULT_OPTS", nil)
 vim.cmd(":FloatermNew fzf")
 vim.fn.setenv("FZF_DEFAULT_COMMAND", fzfcmd)
 vim.fn.setenv("FZF_DEFAULT_OPTS", fzfopts)
end, {})
vim.g.floaterm_opener='edit'
vim.api.nvim_set_keymap('n', '<leader>do', '<cmd> lua vim.diagnostic.open_float()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>f', '<cmd> :FloatermFiles<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<leader>t', '<cmd> :FloatermToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<leader>]', '<cmd> :FloatermNext<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>t', '<cmd> :FloatermToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<c-k>','<cmd> :FloatermNew rg<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>r', '<cmd> :FloatermNew ranger<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<c-c><CR>', '<cmd> :CoqToLine<CR>', { noremap = true, silent = true })

vim.keymap.set("n", "<c-p>", function()
  vim.cmd("bprev")
end, { desc = "Switch to Previous Buffer" })

vim.keymap.set("n", "<c-n>", function()
  vim.cmd("bnext")
end, { desc = "Switch to Next Buffer" })



