{ pkgs }: {
  enable = true;
	clock24 = true;
	plugins = with pkgs.tmuxPlugins; [
		sensible
		yank
		{
			plugin = dracula;
			extraConfig = ''
        set -g @dracula-plugins "battery cpu-usage ram-usage time"
				set -g @dracula-show-battery true
				set -g @dracula-show-powerline true
        set -g @dracula-show-weather false
        set -g @dracula-military-time true
				set -g @dracula-refresh-rate 10
        set -g @dracula-day-month true
        set -g @dracula-show-left-icon session
			'';
		}
	];

	extraConfig = ''
		set -g mouse on
    # Easy config reload
    bind-key R source-file ~/.config/tmux/tmux.conf \; display-message "tmux.conf reloaded."


    # vi is good
    setw -g mode-keys vi

    # mouse behavior
    setw -g mouse on

    bind-key : command-prompt

    bind-key r refresh-client

    bind-key space next-window

    # use vim-like keys for splits and windows
    bind-key -T copy-mode-vi 'y' send -X copy-selection
    # bind-key v split-window -h
    # bind-key s split-window -v
    bind-key h select-pane -L
    bind-key j select-pane -D
    bind-key k select-pane -U
    bind-key l select-pane -R

    # Status Bar
    set-option -g status-interval 1
    set-option -g status-style bg=black
    set-option -g status-style fg=white
    # set -g status-left '#[fg=green]#H #[default]'
    # set -g status-right '%a%l:%M:%S %p#[default] #[fg=blue]%Y-%m-%d'

    # Start windows and panes at 1, not 0
    set -g base-index 1
    setw -g pane-base-index 1

    bind-key C-o rotate-window

    # copy t buff to system clipboard
    bind y run-shell -b "tmux show-buffer | xclip -sel clip -i >> /dev/null" \; display-message "Copied tmux buffer to system clipboard"

    # remap prefix from 'C-b' to 'C-a'
    unbind C-b
    set-option -g prefix C-a
    bind-key C-a send-prefix

    # Set new panes to open in current directory
    bind c new-window -c "#{pane_current_path}"
    bind '"' split-window -c "#{pane_current_path}"
    bind % split-window -h -c "#{pane_current_path}"
	'';
}