module Sensor3d

type Sensor3d (min, max, deviceFile) = 
    member this.read = 
    
(*

class TRIKCONTROL_EXPORT Sensor3d : public QObject
{
	Q_OBJECT

public:
	/// Constructor.
	/// @param min - minimal actual (physical) value returned by sensor. Used to normalize returned values.
	/// @param max - maximal actual (physical) value returned by sensor. Used to normalize returned values.
	/// @param deviceFile - device file for this sensor.
	Sensor3d(int min, int max, QString const &deviceFile);

public slots:
	/// Returns current raw reading of a sensor in a form of vector with 3 coordinates.
	QVector<int> read();

private slots:
	/// Updates current reading when new value is ready.
	void readFile();

private:
	QSharedPointer<QSocketNotifier> mSocketNotifier;
	QVector<int> mReading;
	int mDeviceFileDescriptor;
	int mMax;
	int mMin;
};

}
*)
Sensor3d::Sensor3d(int min, int max, const QString &controlFile)
	: mDeviceFileDescriptor(0)
	, mMax(max)
	, mMin(min)
{
	mReading << 0 << 0 << 0;

	mDeviceFileDescriptor = open(controlFile.toStdString().c_str(), O_SYNC, O_RDONLY);
	if (mDeviceFileDescriptor == -1) {
		qDebug() << "Cannot open input file";
		return;
	}

	mSocketNotifier = QSharedPointer<QSocketNotifier>(
			new QSocketNotifier(mDeviceFileDescriptor, QSocketNotifier::Read, this)
			);

	connect(mSocketNotifier.data(), SIGNAL(activated(int)), this, SLOT(readFile()));
	mSocketNotifier->setEnabled(true);
}

void Sensor3d::readFile()
{
	struct input_event event;

	if (::read(mDeviceFileDescriptor, reinterpret_cast<char *>(&event), sizeof(event)) != sizeof(event)) {
		qDebug() << "incomplete data read";
	} else {
		switch (event.type)
		{
		case EV_ABS:
			switch (event.code)
			{
			case ABS_X:
				mReading[0] = event.value;
				break;
			case ABS_Y:
				mReading[1] = event.value;
				break;
			case ABS_Z:
				mReading[2] = event.value;
				break;
			}
			break;
		case EV_SYN:
			return;
		}
	}
}

QVector<int> Sensor3d::read()
{
	return mReading;
}
