using UnityEngine;
using System.Collections;

public class Fireball : MonoBehaviour
{
	public float Speed = 10.0f;
	public float Distance = 20.0f;

	// Use this for initialization
	void Start ()
	{
		StartCoroutine(FlyAndDie());
	}
	
	// Update is called once per frame
	IEnumerator FlyAndDie()
	{
		// Fly
		float dist = 0.0f;
		Vector3 position = transform.localPosition;
		while (dist < Distance)
		{
			float delta = Speed * Time.deltaTime;
			dist += delta;
			position.z += delta;
			transform.localPosition = position;
			yield return null;
		}

		// Wait for the trail to catch up
		yield return new WaitForSeconds(2.0f);

		// Die
		Destroy(gameObject);
	}
}
