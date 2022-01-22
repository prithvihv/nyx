{ pkgs, ... }:

let
  background = "#6272a4"; 
  foreground = "#f8f8f2"; 
  frame_color = "#ff5555";
  # https://github.com/dracula/dracula-theme/blob/master/README.md#color-palette
in{
  enable = true;
  iconTheme.package = pkgs.numix-icon-theme;
  iconTheme.name = "Numix";
  iconTheme.size = "48";
  settings = {
    global = {
      font = "Iosevka";
      markup = "yes";
      plain_text = "no";
      format = "<b>%s</b>\\n%b";
      transparency = "10";
      ignore_newline = "no";
      show_indicators = "yes";
      separator_color = "#585858";
      sort = "yes";
      alignment = "center";
      bounce_freq = "0";
      word_wrap = "yes";
      indicate_hidden = "yes";
      show_age_threshold = "60";
      idle_threshold = "120";
      geometry = "500x5-10+30";
      shrink = "no";
      line_height = "0";
      notification_height = "100";
      separator_height = "2";
      padding = "8";
      horizontal_padding = "8";
      monitor = "0";
      follow = "mouse";
      sticky_history = "false";
      history_length = "20";
      icon_position = "left";
      max_icon_size = 65;
      startup_notification = "true";
      frame_width = "1";
      frame_color = "#333333";
    };

    shortcuts = {
      close = "ctrl+space";
      close_all = "ctrl+shift+space";
      history = "ctrl+grave";
      context = "ctrl+shift+period";
    };

    urgency_low = {
      background = "${background}";
      foreground = "${foreground}";
      timeout = 10;
    };

    urgency_normal = {
      background = "${background}";
      foreground = "${foreground}";
      timeout = 5;
    };

    urgency_critical = {
      background = "${background}";
      foreground = "${foreground}";
      frame_color = "${frame_color}";
      timeout = 0;
    };
  };
}
