// Sotirios Dimitrakoulakos
import java.math.BigInteger;
import java.util.Scanner;
import java.util.InputMismatchException;

public class Factorizer implements Runnable {
    public Flag factorsFound;
    private BigInteger product;
    private BigInteger factor1;
    private BigInteger factor2;
    private BigInteger step;
    private BigInteger min;
    private BigInteger max;

    private static class Flag {
        private Object lock = new Object();
        private volatile boolean flag = false;
    }

    public Factorizer(Flag found_flag, BigInteger product, BigInteger step, BigInteger min, BigInteger max) {
        this.factorsFound = found_flag;
        this.product = product;
        this.step = step;
        this.min = min;
        this.max = max;
    }

    public BigInteger getFactor1() {
        return factor1;
    }

    public BigInteger getFactor2() {
        return factor2;
    }

    public void run() {
        BigInteger number = min;
        while (number.compareTo(max) < 0 && !factorsFound.flag) {
            synchronized (factorsFound.lock) {
                if (factorsFound.flag) { return; } 
                if (product.remainder(number).compareTo(BigInteger.ZERO) == 0) { 
                    factor1 = number;
                    factor2 = product.divide(factor1);
                    factorsFound.flag = true;
                    return;
                }
                number = number.add(step);
            }
        }
    }

    public static void main(String[] args) {

        Scanner scanner = new Scanner(System.in);
        BigInteger product;
        int num_thread = 0;

        while (true) {
            try {
                System.out.print("Enter the number to be factorized (BigInteger): ");
                String product_input = scanner.nextLine();
                product = new BigInteger(product_input);
                break;
            } catch (NumberFormatException  e) {
                System.out.println("Invalid input. Please enter an integer.");
            }
        }
    
        while (true) {
            boolean in_flag = false;
            try {
                System.out.print("Enter the number of concurrent threads, to be used: ");
                num_thread = scanner.nextInt();
                in_flag = true;
            } catch (InputMismatchException e) {
                System.out.println("Invalid input. Please enter an integer.");
            }
            if (in_flag == true){
                if (num_thread <= 0) { 
                    System.out.println("Invalid input. Please enter a positive integer.");
                } else {
                    break;
                }
            }
        }

        scanner.close();

        long startTime = System.currentTimeMillis();

        Flag found_flag = new Flag();
        BigInteger min = BigInteger.TWO;
        BigInteger max = product;
        BigInteger step = max.divide(BigInteger.valueOf(num_thread));
        Factorizer[] factorizer_array = new Factorizer[num_thread];
        Thread[] thread_array = new Thread[num_thread];

        for (int i = 0; i < num_thread; i++) {
            factorizer_array[i] = new Factorizer(found_flag, product, step, min, max);
            thread_array[i] = new Thread(factorizer_array[i]);
            thread_array[i].start();
            min = min.add(BigInteger.valueOf(i));
        }

        for (int i = 0; i < num_thread; i++) {
            try {
                thread_array[i].join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            if (factorizer_array[i].getFactor1() != null && factorizer_array[i].getFactor2() != null) {
                System.out.println("Factors: " + factorizer_array[i].getFactor1() + ", " + factorizer_array[i].getFactor2());
            }
        }

        if (!found_flag.flag) { System.out.println("No factorization possible"); }

        long endTime = System.currentTimeMillis();
        long computationTime = endTime - startTime;

        System.out.println("Computation time: " + (double) computationTime/1000 + " seconds");
    }
} // I suggest testing with a number of threads of 8 or more, for better results