package com.muthanna.playground.android.counter;

import com.muthanna.playground.android.counter.R;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.Handler;
import android.os.IBinder;
import android.widget.Toast;

/* Implements a simple background "Counting" service, which displays
 * a counter value every 5 seconds.
 */

public class CounterService extends Service {
	private NotificationManager mNM;
    private CounterThread counterthread;
    
	/**
	 * Class for clients to access.  Because we know this service always
	 * runs in the same process as its clients, we don't need to deal with
	 * IPC.
	 */
    public class CounterBinder extends Binder {
        CounterService getService() {
            return CounterService.this;
        }
    }
    
    public class CounterThread extends Thread {
    	public boolean running = false;
    	private int counter = 0;
    	private Handler handler;
    	
    	
    	// Initialize the counter thread with a Handler "h". This
    	// Handler is used to post UI updates.
    	CounterThread(Handler h) {
    		handler = h;
    	}
    	
    	@Override
    	public void run() {
    		synchronized(this) {
    			running = true;
    		}
    			
    		while (isRunning()) {
    			try {
    				Thread.sleep(5000);
    			} catch (InterruptedException e) {
    				// do nothing
    			}
    			
			    incCounter();
			    
			    if (isRunning()) {
			    	/* Post UI update to handler thread. */
			    	
			    	handler.post(new Runnable() {
			    		public void run() {
			    			Toast.makeText(CounterService.this,
			    					"Counter: " + (new Integer(counter).toString()),
			    					Toast.LENGTH_SHORT).show();		
			    		}
			    	});
			    }
    		}
    	}
    	
    	public synchronized int getCounter() {
    		return counter;
    	}
    	
    	public synchronized void incCounter() {
    		counter++;
    	}
    	
    	public synchronized void stopCounter() {
    		running = false;
    		interrupt();
    	}
    	
    	private synchronized boolean isRunning() {
    		return running;
    	}
    }

    @Override
    public void onCreate() {
        mNM = (NotificationManager)getSystemService(NOTIFICATION_SERVICE);

        counterthread = new CounterThread(new Handler());
        counterthread.start();
        
        // Display a notification about us starting.  We put an icon in the status bar.
        showNotification();
    }

    @Override
    public void onDestroy() {
        // Cancel the persistent notification.
        mNM.cancel(R.string.local_service_started);

        counterthread.stopCounter();
        
        try {
        	counterthread.join();
        } catch (InterruptedException e) {
        	// do nothing
        }
       
        // Tell the user we stopped.
        Toast.makeText(this, R.string.local_service_stopped, Toast.LENGTH_SHORT).show();
    }

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }

    // This is the object that receives interactions from clients.  See
    // RemoteService for a more complete example.
    private final IBinder mBinder = new CounterBinder();

    /**
     * Show a notification while this service is running.
     */
    private void showNotification() {
        // In this sample, we'll use the same text for the ticker and the expanded notification
        CharSequence text = getText(R.string.local_service_started);

        // Set the icon, scrolling text and timestamp
        Notification notification = new Notification(R.drawable.icon, text,
                System.currentTimeMillis());

        // The PendingIntent to launch our activity if the user selects this notification
        PendingIntent contentIntent = PendingIntent.getActivity(this, 0,
                new Intent(this, CounterServiceManager.class), 0);

        // Set the info for the views that show in the notification panel.
        notification.setLatestEventInfo(this, getText(R.string.local_service_label),
                       text, contentIntent);

        // Send the notification.
        // We use a layout id because it is a unique number.  We use it later to cancel.
        mNM.notify(R.string.local_service_started, notification);
    }
}
