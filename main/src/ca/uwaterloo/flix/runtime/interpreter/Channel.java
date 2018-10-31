package ca.uwaterloo.flix.runtime.interpreter;


import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;

public class Channel {
  private boolean isOpen;
  private LinkedList<Object> queue;
  private Set<Condition> waitingGetters;
  private Condition waitingSetters;
  private Lock lock;
  private static AtomicInteger GLOBALCOUNTER = new AtomicInteger();
  private int id;

  public Channel() {
    this.isOpen = true;
    this.queue = new LinkedList<>();
    this.lock = new ReentrantLock();
    this.waitingGetters = new HashSet<>();
    this.waitingSetters = lock.newCondition();
    this.id = GLOBALCOUNTER.getAndIncrement();
  }

  public void put(Object e) {
    lock.lock();

    try {
      if (!isOpen) {
        throw new RuntimeException();
      }
      queue.add(e);
      for(Condition c : waitingGetters) {
        c.signalAll();
      }
      waitingGetters.clear();
    } finally {
      lock.unlock();
    }
  }

  public Object get() {
    lock.lock();

    try {
      if (!isOpen) {
        throw new RuntimeException();
      }
      Object e =  queue.poll();
      if (e != null) {
          return e;
      } else {
        try {
          Condition c = lock.newCondition();
          waitingGetters.add(c);
          c.await();
          return get(); //Change this to loop + helpers
        } catch (InterruptedException e2) {
          throw new RuntimeException();
        }
      }
    } finally {
      lock.unlock();
    }
  }

  public Object tryGet() {
    lock.lock();

    try {
      if (!isOpen) {
        throw new RuntimeException();
      }
      return queue.poll();
    } finally {
      lock.unlock();
    }
  }

  public void addGetter(Condition c) {
    lock.lock();
    try {
      waitingGetters.add(c);
    } finally {
      lock.unlock();
    }
  }

  /**
   * Sorts the list by channel id and then locks them all.
   *
   * @param channels the channels to lock
   */
  public static void lockAllChannels(Channel[] channels) {
    Arrays.sort(channels, Comparator.comparing((Channel c) -> c.id));
    for (Channel c : channels) c.lock.lock();
  }

  public static void unlockAllChannels(Channel[] channels) {
    for (Channel c : channels) {
      c.lock.unlock();
    }
  }
}
