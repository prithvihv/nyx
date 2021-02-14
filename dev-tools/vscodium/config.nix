{ 
    diffEditor = {
        ignoreTrimWhitespace = false;
    }; 
    editor = {
        find = {
            cursorMoveOnType = false;
        };
    }; 
    editor = {
        fontFamily = "JetBrains Mono"; 
        fontLigatures = true; 
        lineNumbers = "relative"; 
    };
    explorer = {
        confirmDelete = false; 
        confirmDragAndDrop = false; 
    };
    go = {
        formatTool = "goimports"; 
        # lintFlags = [
        #     "--disable-all"
        #     "--enable=golint"
        #     "--config=~/.gometalinter"
        # ]; 
    };
    javascript = {
        updateImportsOnFileMove = {
            enabled = "always";
        };
    };
    # TODO: rest of this bs!
    # sqltools.autoConnectTo = ; 
    # sqltools.connections = [ ... ]; 
    terminal.integrated.fontFamily = ["JetBrains Mono" "Droid Sans Mono" "monospace" "monospace" "Droid Sans Fallback"];
    # vetur.format.defaultFormatterOptions = { ... }; 
    # vim.enableNeovim = true; 
    # vim.handleKeys = { ... }; 
    # vim.neovimPath = /to/vim; 
    window.zoomLevel = 1; 
    workbench.colorTheme = "Dracula"; 
    # workbench.iconTheme = eq-material-theme-icons-palenight; 
    workbench.sideBar.location = "right"; 
}

