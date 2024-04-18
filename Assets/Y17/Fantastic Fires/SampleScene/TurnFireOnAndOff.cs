using UnityEngine;
using System.Collections;

public class TurnFireOnAndOff : MonoBehaviour
{

	// Use this for initialization
	void Start ()
	{
		StartCoroutine(OnOffForever());
	}

	IEnumerator OnOffForever()
	{
		var animator = GetComponent<Animator>();
		if (animator != null)
		{
			while (true)
			{
				animator.SetBool("Fire", true);
				yield return new WaitForSeconds(2.0f);
				animator.SetBool("Fire", false);
				yield return new WaitForSeconds(2.0f);
			}
		}
	}
}
