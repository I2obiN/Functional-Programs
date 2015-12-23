// Programming Paradigms
// T.Hood

// Use interface/polymorphism instead of inheritence .. interfaces for scala == traits
trait CarT {
  // Car traits
  def fuelGauge(x: FuelGauge): FuelGauge
  def odometer(y: Odometer): Odometer
  // Drive method to drive the car x amount of miles (driver)
  def drive(x: Int)
  // Fills car with x amount of fuel
  def filltank(x: Int)
}

// Car Class
final class Car(fg: FuelGauge, od: Odometer) extends CarT {
  def fuelGauge(fg: FuelGauge) = fg;
  def odometer(od: Odometer) = od;
  private var FuelIntake = 0;

  // Drives the car x amount of miles (driver)
  def drive(x: Int) = {
    var m = x
    // Divide miles by 24 to get number of gallons burned
    var fuelburned = m / 24
    while(fuelburned != 0){fg.decfuel();fuelburned-=1}
    // Increases mileage for each mile driven
    while(m != 0){od.incmileage();m-=1}
  }

  // Fills fuel tank with x gallons amount of petrol
  def filltank(x: Int) = {
    FuelIntake = x
    while(FuelIntake != 0){fg.incfuel();FuelIntake-=1}
  }
  override def toString = {
    "Fuel: " + fg.toString + " Odometer: " + od.toString
  }
}

// Electric Car Class
final class ElectronicCar(fg: ElectronicFuelGauge, od: ElectronicOdometer) extends CarT {
  def fuelGauge(fg: FuelGauge) = new ElectronicFuelGauge(0)
  def odometer(od: Odometer) = new ElectronicOdometer(0)
  private var FuelIntake = 0

  // Drives the car x amount of miles (driver)
  def drive(x: Int) = {
    var m = x
    // Divide miles by 24 to get number of gallons burned
    var fuelburned = m / 24
    while(fuelburned != 0){fg.decfuel();fuelburned-=1}
    // Increases mileage for each mile driven
    while(m != 0){od.incmileage();m-=1}
  }

  // Fills fuel tank with x gallons amount of petrol
  def filltank(x: Int) = {
    FuelIntake = x
    while(FuelIntake != 0){fg.incfuel();FuelIntake-=1}
  }
  override def toString = {
    "Fuel: " + fg.toString + " Odometer: " + od.toString
  }
}

// Mechanical Car Class
final class MechanicalCar(fg: AnalogFuelGauge, od: MechanicalOdometer) extends CarT {
  def fuelGauge(fg: FuelGauge) = new AnalogFuelGauge(0)
  def odometer(od: Odometer) = new MechanicalOdometer(0)
  private var FuelIntake = 0

  // Drives car x amount of miles
  def drive(x: Int) = {
    var m = x
    // Divide miles by 24 to get number of gallons burned
    var fuelburned = m / 24
    while(fuelburned != 0){fg.decfuel();fuelburned-=1}
    // Increases mileage for each mile driven
    while(m != 0){od.incmileage();m-=1}
  }

  // Fills fuel tank with x gallons amount of petrol
  def filltank(x: Int) = {
    FuelIntake = x
    while(FuelIntake != 0){fg.incfuel();FuelIntake-=1}
  }

  override def toString = {
    "Fuel: " + fg.toString + " Odometer: " + od.toString
  }
}

// Odometer Class - Abstract Polymorphism
abstract class Odometer(x: Int) {
  def status(): String
  def incmileage()
  def resetmileage()
}

// Only difference to Mechanical is presentation
final class ElectronicOdometer(x:Int) extends Odometer(x) {
  private var Mileage: Int = x
  def status() = {
    Mileage.toString
  }

  // Increases mileage
  def incmileage() = {
    if(Mileage < 999999){
      Mileage += 1
    }
      else{Mileage = 0}
  }

  // resets counter
  def resetmileage() = {
    Mileage = 0
  }

  override def toString = {
    Mileage.toString
  }
}

// Displays mileage in mechanical form
final class MechanicalOdometer(x:Int) extends Odometer(x) {
  private var Mileage: Int = x
  def status()= Mileage match {
    case x if x == 0 && x < 1 => "000000"
    case x if x < 10 && x > 0 => "00000".concat(x.toString)
    case x if x < 100 && x > 10 => "0000".concat(x.toString)
    case x if x < 1000 && x > 100 => "000".concat(x.toString)
    case x if x < 10000 && x > 1000 => "00".concat(x.toString)
    case x if x < 100000 && x > 10000 => "0".concat(x.toString)
    case x if x > 100000 => Mileage.toString
  }

  // Increases mileage, loops to zero if > 999999
  def incmileage() = {
    if(Mileage < 999999){
      Mileage += 1
    }
    else{Mileage = 0}
  }
  // Resets counter
  def resetmileage() = {
    Mileage = 0
  }
  override def toString = Mileage match {
    case x if x == 0 && x < 1 => "000000"
    case x if x < 10 && x > 0 => "00000".concat(x.toString)
    case x if x < 100 && x > 10 => "0000".concat(x.toString)
    case x if x < 1000 && x > 100 => "000".concat(x.toString)
    case x if x < 10000 && x > 1000 => "00".concat(x.toString)
    case x if x < 100000 && x > 10000 => "0".concat(x.toString)
    case x if x > 100000 => x.toString
  }
}

// Fuel Gauge class
abstract class FuelGauge(x: Int){
  def status(): String
  def incfuel()
  def decfuel()
}

// Analog Fuel Gauge displays level of fuel only
final class AnalogFuelGauge(x: Int) extends FuelGauge(x){
  private var Gallons: Int = x
  def status() = Gallons match {
    case x if x == 15 => "Full."
    case x if x < 15 && x > 11 => "3/4 Full."
    case x if x < 12 && x > 7 => "1/2 Full."
    case x if x < 8 && x > 3 => "1/4 Full."
    case x if x < 3 => "Empty."
  }

  // Increase fuel by one gallon
  def incfuel() = {
    if (Gallons < 15) {
      Gallons += 1
    }
    else {
      println("Fuel Tank Full!")
    }
  }

  // Burns a gallon of fuel
  def decfuel()= {
      if(Gallons > 0){
      Gallons-=1
      }
      else {
        println("Fuel Tank Empty!")
      }
    }
  override def toString = Gallons match {
    case x if x == 15 => "Full."
    case x if x < 15 && x > 11 => "3/4 Full."
    case x if x < 12 && x > 7 => "1/2 Full."
    case x if x < 8 && x > 3 => "1/4 Full."
    case x if x < 3 => "Empty."
    }
  }

// Electronic Fuel Gauge class, same as before but displays an integer
final class ElectronicFuelGauge(x: Int) extends FuelGauge(x){
  private var Gallons: Int = x
  def status() = {
    Gallons.toString + "-Gal."
  }
  def incfuel() = {
    if (Gallons < 15) {
      Gallons += 1
    }
    else {
      println("Fuel Tank Full!")
    }
  }
  def decfuel()= {
    if(Gallons > 0){
      Gallons-=1
    }
    else {
      println("Fuel Tank Empty!")
    }
  }
  override def toString = {
    Gallons.toString + "-Gal."
  }
}

// Car Testing - Electronic
val car1 = new ElectronicCar(new ElectronicFuelGauge(0), new ElectronicOdometer(0))
// Won't fill beyond 15
car1.filltank(20)
car1
car1.drive(72)
car1
// Car Testing - Mechanical
val car2 = new MechanicalCar(new AnalogFuelGauge(0), new MechanicalOdometer(0))
// Won't fill beyond 15
car2.filltank(20)
car2
car2.drive(48)
car2
car2.drive(148)
car2
// Car Testing - Both
val car3 = new Car(new ElectronicFuelGauge(0), new MechanicalOdometer(0))
// Won't fill beyond 15
car3.filltank(20)
car3
car3.drive(48)
car3
car3.drive(148)
car3
