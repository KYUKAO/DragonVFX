using UnityEngine;
using System.Collections;

public class SpawnFireballs : MonoBehaviour
{
	public GameObject[] FireballPrefabs;
	public float Delay = 2.0f;

	// Use this for initialization
	void Start()
	{
		StartCoroutine(SpawnForever());
	}
	
	// Update is called once per frame
	IEnumerator SpawnForever()
	{
		while (true)
		{
			var prefab = FireballPrefabs[Random.Range(0, FireballPrefabs.Length)];
			Vector3 offset = Random.insideUnitSphere;
			var fireball = GameObject.Instantiate(prefab, transform.position + offset, transform.rotation) as GameObject;
			fireball.transform.SetParent(transform);
			yield return new WaitForSeconds(Delay);
		}
	}
}
