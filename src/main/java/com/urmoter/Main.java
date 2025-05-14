package com.urmoter;

import net.dv8tion.jda.api.*;
import net.dv8tion.jda.api.entities.*;
import net.dv8tion.jda.api.entities.channel.middleman.MessageChannel;
import net.dv8tion.jda.api.events.message.*;
import net.dv8tion.jda.api.hooks.*;
import net.dv8tion.jda.api.requests.GatewayIntent;
import org.jetbrains.annotations.NotNull;

import java.util.Scanner;

class BotListen extends ListenerAdapter {
    @Override
    public void onMessageReceived(@NotNull MessageReceivedEvent event) {
        if (event.getAuthor().isBot()) return;

        Message msg = event.getMessage();
        String content = msg.getContentRaw();

        if (content.toLowerCase().contains("run")) {
            MessageChannel channel = event.getChannel();
            channel.sendMessage(
                    "Run Forrest, Run! https://tenor.com/view/hahaha-run-forrest-gump-tom-hanks-gif-4170876"
            ).queue();
        } else if (content.contains("<@1371854279283052635>")) {
            event.getChannel().sendMessage("Yes, Jennay?").setMessageReference(event.getMessageId()).queue();
        } else if (content.equals("https://cdn.discordapp.com/attachments/1361579296258265149/1371327468644073482/WideAmongUsGaySex-1x.gif")) {
            event.getChannel().sendMessage("https://cdn.discordapp.com/attachments/1361579296258265149/1371327468644073482/WideAmongUsGaySex-1x.gif").queue();
        }
    }
}

class Main {
    public static void main(String[] args) throws Exception {
        Scanner input = new Scanner(System.in);
        System.out.println("Bot Token:");
        String token = input.nextLine();

        JDA instance = JDABuilder.createLight(token, GatewayIntent.GUILD_MESSAGES, GatewayIntent.MESSAGE_CONTENT, GatewayIntent.GUILD_MEMBERS).addEventListeners(new BotListen()).build();
    }
}