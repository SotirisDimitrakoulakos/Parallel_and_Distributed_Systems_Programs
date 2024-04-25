// Sotirios Dimitrakoulakos
// [Replace this comment with your own name.]

// [Do necessary modifications of this file.]

package paradis.assignment3;

// [You are welcome to add some import statements.]

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ForkJoinPool;

public class Program1 {
    final static int NUM_WEBPAGES = 100;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    // [You are welcome to add some variables.]

    private static BlockingQueue<WebPage> queue1 = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> queue2 = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> queue3 = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> queue4 = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static ForkJoinPool thread_pool = ForkJoinPool.commonPool();

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void initialize() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
            try {
                queue1.put(webPages[i]);
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void downloadWebPages() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage web_page = queue1.take();
                thread_pool.submit(() -> {
                    try {
                        web_page.download();
                        queue2.put(web_page);
                    } catch (Exception exception) {
                        System.out.println(exception);
                    }
                });
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void analyzeWebPages() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage web_page = queue2.take();
                thread_pool.submit(() -> {
                    try {
                        web_page.analyze();
                        queue3.put(web_page);
                    } catch (Exception exception) {
                        System.out.println(exception);
                    }
                });
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void categorizeWebPages() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage web_page = queue3.take();
                thread_pool.submit(() -> {
                    try {
                        web_page.categorize();
                        queue4.put(web_page);
                    } catch (Exception exception) {
                        System.out.println(exception);
                    }
                });
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void presentResult() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage web_page = queue4.take();
                System.out.println(web_page);
            } catch (Exception exception) {
                System.out.println(exception);
            }
        }
    }

    public static void main(String[] args) {
        // Initialize the list of webpages.
        initialize();

        // Start timing.
        long start = System.nanoTime();

        // Do the work.
        downloadWebPages();
        analyzeWebPages();
        categorizeWebPages();

        // Stop timing.
        long stop = System.nanoTime();

        // Present the result.
        presentResult();

        // Present the execution time.
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}