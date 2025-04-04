local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "filter=blob:none", "branch=stable", lazyrepo, lazypath })
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

vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("lazy").setup({
  spec = {
    {
      "nyoom-engineering/oxocarbon.nvim",
      priority = 1000,
      config = function()
        vim.cmd("colorscheme oxocarbon")
      end
    },
    'nvim-treesitter/nvim-treesitter',
  },
})

local opts = {
  autowrite = true,
  clipboard = "unnamedplus",
  completeopt = "menu,menuone,noselect",
  conceallevel = 2,
  confirm = true,
  cursorline = true,
  expandtab = true,
  fillchars = { foldopen = "", foldclose = "", fold = " ", foldsep = " ", diff = "╱", eob = " " },
  foldlevel = 99,
  formatoptions = "jcroqlnt",
  grepformat = "%f:%l:%c:%m",
  grepprg = "rg vimgrep",
  ignorecase = true,
  inccommand = "nosplit",
  jumpoptions = "view",
  laststatus = 3,
  linebreak = true,
  list = true,
  mouse = "a",
  number = true,
  pumblend = 10,
  pumheight = 10,
  relativenumber = true,
  ruler = false,
  scrolloff = 4,
  sessionoptions = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp", "folds" },
  shiftround = true,
  shiftwidth = 2,
  showmode = false,
  sidescrolloff = 8,
  signcolumn = "yes",
  smartcase = true,
  smartindent = true,
  spelllang = { "en" },
  splitbelow = true,
  splitkeep = "screen",
  splitright = true,
  tabstop = 2,
  termguicolors = true,
  timeoutlen = 300,
  undofile = true,
  undolevels = 10000,
  updatetime = 200,
  virtualedit = "block",
  wildmode = "longest:full,full",
  winminwidth = 5,
  wrap = false,
  swapfile = false,
  smoothscroll = true,
  foldmethod = "expr",
  foldtext = ""
}
for k, v in pairs(opts) do
  vim.opt[k] = v
end

vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")
