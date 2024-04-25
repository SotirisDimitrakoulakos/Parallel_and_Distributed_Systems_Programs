// Sotirios Dimitrakoulakos
// [Replace this comment with your own name.]

// [Do necessary modifications of this file.]

package paradis.assignment3;

//[You are welcome to add some import statements.]

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

// [You are welcome to add some import statements.]

public class MyExecutor implements ExecutorService{

	private final static int NUM_THREADS = Runtime.getRuntime().availableProcessors(); //Can change it to whatever number the programmer
																				// wants, but this is the most efficient
	private final List<Thread> thread_pool = new ArrayList<>();
	private static BlockingQueue<Runnable> queue = new LinkedBlockingQueue<>();
	private boolean isShutdown = false;
	
	public MyExecutor() {
        for (int i = 0; i < NUM_THREADS; i++) {
        	executeTask task_execution = new executeTask();
            Thread thread = new Thread(task_execution);
            thread_pool.add(thread);
            thread.start();
        }
	}
	
	public class executeTask implements Runnable {
		
		public void run() {
			while (true) {
				try {
			        if (isShutdown==true && queue.isEmpty()) {
			            break;
			        }
			        Runnable task = queue.take();
			        task.run();
				} catch (Exception exception) {
					System.out.println(exception);
					break;	
				}
			}
		}
		
	}

	@Override
	public void execute(Runnable command) {
		// TODO Auto-generated method stub
        if (command == null) {
            throw new NullPointerException();
        }
        if (isShutdown) {
            throw new RejectedExecutionException("Executor is shut down");
        }
        try {
        	queue.put(command);
        } catch (Exception exception) {
			System.out.println(exception);
        }
	}

	@Override
	public void shutdown() {
		// TODO Auto-generated method stub
        isShutdown = true;
        for (Thread thread : thread_pool) {
            thread.interrupt();
        }
	}

	@Override
	public List<Runnable> shutdownNow() {
		// TODO Auto-generated method stub
        isShutdown = true;
        List<Runnable> unfinishedTasks = new ArrayList<>();
        for (Thread thread : thread_pool) {
            if (thread.isAlive()) {unfinishedTasks.add(() -> {});}
        }
        shutdown();
        return unfinishedTasks;
	}

	@Override
	public boolean isShutdown() {
		// TODO Auto-generated method stub
		return isShutdown;
	}

	@Override
	public boolean isTerminated() {
		// TODO Auto-generated method stub
		return isShutdown && thread_pool.stream().noneMatch(Thread::isAlive);
	}

	@Override
	public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException {
		// TODO Auto-generated method stub
        long endTime = System.currentTimeMillis() + unit.toMillis(timeout);
        while (System.currentTimeMillis() < endTime) {
            if (isTerminated()) {
                return true;
            }
            Thread.sleep(100);
        }
        return false;
	}

	@Override
	public <T> Future<T> submit(Callable<T> task) {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("submit(Callable<T> task) is not supported");
	}

	@Override
	public <T> Future<T> submit(Runnable task, T result) {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("submit(Runnable task, T result) is not supported");
	}

	@Override
	public Future<?> submit(Runnable task) {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("submit(Runnable task) is not supported");
	}

	@Override
	public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks) throws InterruptedException {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("invokeAll is not supported");
	}

	@Override
	public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("invokeAll with timeout is not supported");
	}

	@Override
	public <T> T invokeAny(Collection<? extends Callable<T>> tasks) throws InterruptedException, ExecutionException {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("invokeAny is not supported");
	}

	@Override
	public <T> T invokeAny(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) {
		// TODO Auto-generated method stub
        throw new UnsupportedOperationException("invokeAny with timeout is not supported");
	}
	
}