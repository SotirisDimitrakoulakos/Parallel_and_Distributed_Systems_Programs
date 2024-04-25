// Sotirios Dimitrakoulakos

package paradis.assignment4;

import java.net.Socket;
import java.net.ServerSocket;
import java.net.SocketAddress;
import java.io.PrintWriter;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

class ChatServer implements Runnable {
    private final static int PORT = 8000;
    private final static int MAX_CLIENTS = 5;
    private final static Executor thread_pool = Executors.newFixedThreadPool(MAX_CLIENTS);
    private final BlockingQueue<Message> message_queue = new LinkedBlockingQueue<>();
    private static final Map<SocketAddress, PrintWriter> clients = new ConcurrentHashMap<>(); // address and PrintWriter object (no locks)
    private final Socket clientSocket;
    private String clientName = "";

    private ChatServer(Socket clientSocket) {
        this.clientSocket = clientSocket;
    }
    
    private static class Message {
        private final String name;
        private final String content;
        private final long timestamp;

        public Message(String sender, String content) {
            this.name = sender;
            this.content = content;
            this.timestamp = System.currentTimeMillis();
        }

        public String getName() {
            return name;
        }

        public String getContent() {
            return content;
        }

        public long getTimestamp() {
            return timestamp;
        }
    }

    public void run() {
        SocketAddress remoteSocketAddress = clientSocket.getRemoteSocketAddress();
        SocketAddress localSocketAddress = clientSocket.getLocalSocketAddress();
        System.out.println("Accepted client " + remoteSocketAddress + " (" + localSocketAddress + ").");
        BufferedReader socketReader = null;
        try {
        	socketReader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
            String threadInfo = " (" + Thread.currentThread().getName() + ").";
            clientName = socketReader.readLine();
            while (true) {
                String inputLine = socketReader.readLine();
                if (inputLine == null) {
                    break;
                }
                message_queue.put(new Message(clientName, inputLine));
                broadcast_messages();
            }
            System.out.println("Closing connection " + remoteSocketAddress + " (" + localSocketAddress + ").");
        } catch (Exception exception) {
            System.out.println(exception);
        } finally {
            try {
                clients.remove(remoteSocketAddress);
                if (socketReader != null) {socketReader.close();}
                if (clientSocket != null) {clientSocket.close();}
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }

    private void broadcast_messages() {
        while (true) {
            try {
                // Dequeue messages from the messageQueue and broadcast them.
                Message message = message_queue.take();
                clients.forEach((address, writer) -> {
                    synchronized (writer) {
                        writer.println(message.getName() + ": " + message.getContent());
                        System.out.println("Sent: " + message.getContent() + "\" from " + message.getName() + "\" to " + address + "\" at " + message.getTimestamp());
                    }
                });
            } catch (InterruptedException e) {
                System.out.println(e);
            }
        }
    }

    public static void main(String[] args) {
        System.out.println("ChatServer started.");
        ServerSocket serverSocket = null;
        Socket clientSocket = null;
        try {
            serverSocket = new ServerSocket(PORT);
            SocketAddress serverSocketAddress = serverSocket.getLocalSocketAddress();
            System.out.println("Listening (" + serverSocketAddress + ").");
            while (true) {
                clientSocket = serverSocket.accept();
                clients.put(clientSocket.getRemoteSocketAddress(), new PrintWriter(clientSocket.getOutputStream(), true));
                thread_pool.execute(new ChatServer(clientSocket));
            }
        } catch (Exception exception) {
            System.out.println(exception);
        } finally {
            try {
                if (serverSocket != null)
                    serverSocket.close();
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }


}