/*
  Sirens for Arkin.
*/

int current = 3;
int dir = 0;


const float baseTemp = 20.0;

void setup() {
  for (int pin=2; pin <= 5; ++pin) {
    pinMode(pin, OUTPUT);
  }
  
  // For the serial console. Used to send debug
  // messages.
  Serial.begin(9600);
}

void switchOffLEDs() {
  digitalWrite(3, LOW);
  digitalWrite(4, LOW);
  digitalWrite(5, LOW);
}

void blinkNextLED() {
  digitalWrite(current, HIGH);
  if (dir == 0) {
    if (++current == 5) dir = 1;
  } else {
    if (--current == 3) dir = 0;
  }
  delay(150);
  switchOffLEDs();
}

float getTemperature() {
  const int sensorPin = A0;

  float voltage = (analogRead(sensorPin) / 1024.0) * 5.0;
  float temp = (voltage- .5) * 100;
  
  Serial.print("Voltage: ");
  Serial.print(voltage);
  Serial.print(", Temperature: ");
  Serial.println(temp);
  
  return temp;
}
  
void loop() {
  // Click "Serial Monitor" on top right of IDE to
  // see logs.
  float temp = getTemperature();
  
  if (temp >= 24) {
    blinkNextLED();
  } else {
    switchOffLEDs();
  }    
}
